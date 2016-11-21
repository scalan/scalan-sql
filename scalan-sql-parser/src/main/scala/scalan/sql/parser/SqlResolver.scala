package scalan.sql.parser

import scala.annotation.tailrec
import scalan.sql.parser.SqlAST._
import scalan.util.ReflectionUtil

class SqlResolver(val schema: Schema) {
  def table(name: String) = schema.table(name)
  def indices(tableName: String) = schema.indices(tableName)

  case class Scope(var ctx: Context, outer: Option[Scope], nesting: Int, name: String) {
    def resolve(unresolved: UnresolvedAttribute): ResolvedAttribute =
      ctx.resolveColumn(unresolved).getOrElse {
        outer match {
          case Some(s) =>
            s.resolve(unresolved)
          case None =>
            throw SqlException(s"Failed to resolve column $unresolved")
        }
      }

    def lookup(col: ResolvedAttribute): Binding = {
      ctx.lookup(col) match {
        case Some(b) => b
        case None => {
          outer match {
            case Some(s: Scope) =>
              s.lookup(col)
            case _ =>
              throw SqlException(s"Failed to lookup column $col")
          }
        }
      }
    }
  }

  var currScope: Scope = Scope(GlobalContext, None, 0, "scalan")

  def withContext[A](op: Operator)(block: => A) = {
    currScope = Scope(currScope.ctx, Some(currScope), currScope.nesting + 1, if (currScope.nesting == 0) "r" else "r" + currScope.nesting.toString)
    currScope.ctx = buildContext(op)
    val result = block
    currScope = currScope.outer.get
    result
  }

  def resolveAggregates(parent: Operator, groupedBy: ExprList, columns: List[SelectListElement]): Operator = {
    def simplifyAgg(x: Expression): Expression = x match {
      case AggregateExpr(Count, false, _) =>
        CountAllExpr
      case AggregateExpr(op, distinct, value) =>
        val normalizedValue = simplifyAgg(resolveExpr(value))
        op match {
          case Avg =>
            val sum = AggregateExpr(Sum, distinct, normalizedValue)
            val count = if (distinct)
              AggregateExpr(Count, distinct, normalizedValue)
            else
              CountAllExpr
            BinOpExpr(Divide, CastExpr(sum, DoubleType), CastExpr(count, DoubleType))
          case _ =>
            AggregateExpr(op, distinct, normalizedValue)
        }
      case x: Literal => x
      case x if ReflectionUtil.isSingleton(x) => x
      case p: Product =>
        val constructor = x.getClass.getConstructors.apply(0)
        val members = p.productIterator.map {
          case e: Expression => simplifyAgg(e)
          case x => x.asInstanceOf[AnyRef]
        }.toArray
        constructor.newInstance(members: _*).asInstanceOf[Expression]
    }

    def extractAggregateExprs(expr: Expression): List[AggregateExpr] = expr match {
      case agg: AggregateExpr =>
        List(agg)
      case BinOpExpr(_, l, r) =>
        extractAggregateExprs(l) ++ extractAggregateExprs(r)
      case LikeExpr(l, r, escape) =>
        extractAggregateExprs(l) ++ extractAggregateExprs(r) ++ escape.toList.flatMap(extractAggregateExprs)
      case NegExpr(opd) =>
        extractAggregateExprs(opd)
      case NotExpr(opd) =>
        extractAggregateExprs(opd)
      case _: Literal | _: Parameter =>
        Nil
      case CastExpr(exp, typ) =>
        extractAggregateExprs(exp)
      case ref: UnresolvedAttribute =>
        Nil
      case SubstrExpr(str, from, len) =>
        extractAggregateExprs(str) ++ extractAggregateExprs(from) ++ extractAggregateExprs(len)
      case CaseWhenExpr(list) =>
        list.flatMap(extractAggregateExprs)
      case InListExpr(sel, list) =>
        extractAggregateExprs(sel) ++ list.flatMap(extractAggregateExprs)
      // case SelectExpr(s) =>
      // case InExpr(sel, query) =>
      case FuncExpr(name, args) =>
        args.flatMap(extractAggregateExprs)
      case _ =>
        throw new SqlException(s"Don't know how to extract aggregate parts from a ${expr.getClass.getSimpleName}")
    }

    columns.find {
      case ProjectionColumn(expr, _) =>
        !(containsAggregates(expr) || groupedBy.contains(expr))
      case UnresolvedStar(_) =>
        true
    } match {
      case Some(projectionColumn) =>
        throw new SqlException(s"Non-aggregate column $projectionColumn in an aggregate query")
      case None =>
        val normalizedColumns = columns.map {
          case col: ProjectionColumn => col.mapExpr(simplifyAgg)
          case _ => throw new SqlException("Impossible")
        }

        val aggregates = normalizedColumns.flatMap { col =>
          extractAggregateExprs(col.expr)
        }.distinct

        val aggregate = Aggregate(parent, groupedBy.map(resolveExpr), aggregates)

        withContext(aggregate) {
          val resolvedColumns = normalizedColumns.map(_.mapExpr(resolveExpr))
          Project(aggregate, resolvedColumns)
        }
    }
  }

  def resolveOperator(op: Operator): Operator = op match {
    case Scan(tableName, _) =>
      op
    case Distinct(table) =>
      Distinct(resolveOperator(op))
    case Union(left, right) =>
      Union(resolveOperator(left), resolveOperator(right))
    case Except(left, right) =>
      Except(resolveOperator(left), resolveOperator(right))
    case Intersect(left, right) =>
      Intersect(resolveOperator(left), resolveOperator(right))
    case TableAlias(table, alias) =>
      // TODO alias should be removed during resolution, but we need to make sure AliasContext is produced
      // For now aliases are just ignored in ScalanSqlBridge
      TableAlias(resolveOperator(table), alias)
    case Filter(parent, predicate) =>
      val parent1 = resolveOperator(parent)
      val predicate1 = withContext(parent1) {
        resolveExpr(predicate)
      }
      pushdownFilter(parent1, predicate1)
    case Project(parent, columns) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        if (columns.exists {
          case col: ProjectionColumn =>
            containsAggregates(col)
          case _: UnresolvedStar =>
            false
        })
          resolveAggregates(parent1, Nil, columns)
        else {
          val columns1 = columns.flatMap {
            case ProjectionColumn(expr, alias) =>
              List(ProjectionColumn(resolveExpr(expr), alias))
            case UnresolvedStar(qualifier) =>
              currScope.ctx.resolveStar(qualifier).getOrElse {
                throw new SqlException(s"Can't resolve ${qualifier.fold("")(_ + ".")}*")
              }
          }
          Project(parent1, columns1)
        }
      }
    case GroupBy(Project(parent, columns), groupedBy) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        resolveAggregates(parent1, groupedBy, columns)
      }
    case OrderBy(parent, columns) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        OrderBy(parent1, columns.map(s => s.copy(expr = resolveExpr(s.expr))))
      }
    case Limit(parent, limit) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        Limit(parent1, resolveExpr(limit))
      }
    case SubSelect(parent) =>
      resolveOperator(parent)
    case Join(left, right, joinType, spec) =>
      val left1 = resolveOperator(left)
      val right1 = resolveOperator(right)
      val condition1 = spec match {
        case On(condition) =>
          withContext(left1) {
            withContext(right1) {
              resolveExpr(condition)
            }
          }
        case Using(columns) =>
          def resolvedColumns() = columns.map(name => resolveExpr(UnresolvedAttribute(None, name)))
          val cLeft = withContext(left1) {
            resolvedColumns()
          }
          val cRight = withContext(right1) {
            resolvedColumns()
          }
          (cLeft, cRight).zipped.map(BinOpExpr(Eq, _, _)).reduce(BinOpExpr(And, _, _))
        case Natural =>
          // requires implementing operatorType, won't support it yet
          throw new NotImplementedError("Natural joins not supported yet")
      }
      Join(left1, right1, joinType, On(condition1))
    case CrossJoin(outer, inner) =>
      CrossJoin(resolveOperator(outer), resolveOperator(inner))
    case UnionJoin(outer, inner) =>
      UnionJoin(resolveOperator(outer), resolveOperator(inner))
    case _ =>
      throw new NotImplementedError(s"Can't resolve operator\n$op")
  }

  def pushdownFilter(parent: Operator, predicate: Expression): Operator = {
    val clauses = conjunctiveClauses(predicate)

    // returns the list of clauses which remain unhandled
    def doPushdown(op: Operator, clauses: List[Expression]): (Operator, List[Expression]) = op match {
      // TODO handle other op cases
      case Join(left, right, joinType, joinSpec) =>
        val (clausesDependingOnRight, leftOnlyClauses) = clauses.partition(depends(right, _))
        val left1 = doPushdownWithNoRemainingClauses(left, leftOnlyClauses)
        val (clausesDependingOnBoth, rightOnlyClauses) = clausesDependingOnRight.partition(depends(left, _))
        val right1 = doPushdownWithNoRemainingClauses(right, rightOnlyClauses)
        (Join(left1, right1, joinType, joinSpec), clausesDependingOnBoth)
      case Filter(parent, condition) =>
        doPushdown(parent, (clauses ++ conjunctiveClauses(condition)).distinct)
      case _ => (op, clauses)
    }

    def doPushdownWithNoRemainingClauses(op: Operator, clauses: List[Expression]) = {
      val (pushedDown, remainingClauses) = doPushdown(op, clauses)
      remainingClauses match {
        case Nil =>
          pushedDown
        case _ =>
          // TODO check if repeated filters perform better due to lack of shortcutting
          val remainingPredicate = remainingClauses.reduce(BinOpExpr(And, _, _))
          Filter(pushedDown, remainingPredicate)
      }
    }

    doPushdownWithNoRemainingClauses(parent, clauses)
  }

  def resolveExpr(expr: Expression): Expression = expr match {
    case SelectExpr(op) =>
      SelectExpr(resolveOperator(op))
    case BinOpExpr(op, left, right) =>
      BinOpExpr(op, resolveExpr(left), resolveExpr(right))
    case agg @ AggregateExpr(op, distinct, value) =>
      currScope.ctx.resolveAggregate(agg).getOrElse {
        throw new SqlException(s"Aggregate $agg not found")
      }
      // AggregateExpr(op, distinct, resolveExpr(value))
    case LikeExpr(left, right, escape) =>
      LikeExpr(resolveExpr(left), resolveExpr(right), escape.map(resolveExpr))
    case InListExpr(expr, list) =>
      InListExpr(resolveExpr(expr), list.map(resolveExpr))
    case InExpr(left, subquery) =>
      InExpr(resolveExpr(left), resolveOperator(subquery))
    case ExistsExpr(op) =>
      ExistsExpr(resolveOperator(op))
    case NegExpr(opd) =>
      NegExpr(resolveExpr(opd))
    case NotExpr(opd) =>
      NotExpr(resolveExpr(opd))
    case SubstrExpr(str, from, len) =>
      SubstrExpr(resolveExpr(str), resolveExpr(from), resolveExpr(len))
    case IsNullExpr(opd) =>
      IsNullExpr(resolveExpr(opd))
    case FuncExpr(name, params) =>
      FuncExpr(name, params.map(resolveExpr))
    case CastExpr(expr, to) =>
      CastExpr(resolveExpr(expr), to)
    case CaseWhenExpr(list) =>
      CaseWhenExpr(list.map(resolveExpr))
    case Literal(_, _) | NullLiteral | _: ResolvedAttribute | _: Parameter =>
      expr
    case unresolved: UnresolvedAttribute =>
      currScope.resolve(unresolved)
  }

  def lookup(col: ResolvedAttribute): Binding = currScope.lookup(col)

  def tablesInNestedSelects(e: Expression): Set[Table] = {
    e match {
      case BinOpExpr(op, l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case ExistsExpr(q) => tables(q)
      case LikeExpr(l, r, _) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case NegExpr(opd) => tablesInNestedSelects(opd)
      case NotExpr(opd) => tablesInNestedSelects(opd)
      case CastExpr(exp, typ) => tablesInNestedSelects(exp)
      case SelectExpr(s) => tables(s)
      case InExpr(s, q) => tablesInNestedSelects(s) ++ tables(q)
      case _ => Set()
    }
  }

  def tables(op: Operator): Set[Table] = {
    op match {
      case Join(left, right, _, _) => tables(left) ++ tables(right)
      case Scan(t, _) => Set(table(t))
      case OrderBy(p, _) => tables(p)
      case Aggregate(p, _, _) => tables(p)
      case Filter(p, predicate) => tables(p) ++ tablesInNestedSelects(predicate)
      case Project(p, columns) => tables(p)
      case TableAlias(t, a) => tables(t)
      case SubSelect(p) => tables(p)
      case _ => throw new NotImplementedError(s"tables($op)")
    }
  }

  sealed trait Selector
  case class Field(name: String) extends Selector
  case class Index(i: Int) extends Selector
  case object First extends Selector
  case object Second extends Selector

  case class Binding(scope: String, path: List[Selector])

  // This is SQLite-specific implementation, override for other DBs
  /** call only after ensuring there is no column with this name! */
  protected def isImplicitRowidColumn(columnName: String, table: Table) =
    (columnName == "rowid" || columnName == "_rowid_" || columnName == "oid") && !table.withoutRowId

  def resolvedTableAttributeByName(table: Table, tableId: Int, columnName: String) = {
    table.columns.indexWhere(_.name == columnName) match {
      case -1 if !isImplicitRowidColumn(columnName, table) =>
        None
      case i =>
        Some(ResolvedTableAttribute(table, tableId, i))
    }
  }

  abstract class Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute]
    def resolveAggregate(agg: AggregateExpr): Option[ResolvedAttribute] = None
    def resolveStar(qualifier: Option[String]): Option[List[ProjectionColumn]]
    def lookup(resolved: ResolvedAttribute): Option[Binding]
    def lookup(unresolved: UnresolvedAttribute): Option[Binding] = resolveColumn(unresolved).flatMap(lookup)

    val scope = currScope
  }

  case object GlobalContext extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = None
    def resolveStar(qualifier: Option[String]) = None
    def lookup(resolved: ResolvedAttribute): Option[Binding] = None
  }

  case class TableContext(table: Table, id: Int) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] =
      ifQualifierMatches(ref.table) {
        resolvedTableAttributeByName(table, id, ref.name)
      }

    def resolveStar(qualifier: Option[String]) =
      ifQualifierMatches(qualifier) {
        val projectionColumns = table.columns.indices.toList.map { i =>
          val attr = ResolvedTableAttribute(table, id, i)
          ProjectionColumn(attr, None)
        }
        Some(projectionColumns)
      }

    def ifQualifierMatches[A](qualifier: Option[String])(body: => Option[A]): Option[A] = {
      if (qualifier.isEmpty || qualifier == Some(table.name)) body else None
    }

    def lookup(resolved: ResolvedAttribute): Option[Binding] = resolved match {
      case rta @ ResolvedTableAttribute(`table`, `id`, index) =>
        Some(Binding(scope.name, List(Field(rta.name))))
      case ResolvedProjectedAttribute(parent: ResolvedAttribute, _, _) =>
        lookup(parent)
      case _ => None
    }

    override def toString: String = s"TableContext(${table.name}{$id})"
  }

  case class JoinContext(outer: Context, inner: Context) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      (outer.resolveColumn(ref), inner.resolveColumn(ref)) match {
        case (Some(b), None) =>
          Some(b)
        case (None, Some(b)) =>
          Some(b)
        case (Some(_), Some(_)) =>
          throw SqlException(s"Ambiguous reference to $ref")
        case _ => None
      }
    }

    def resolveStar(qualifier: Option[String]) =
      (outer.resolveStar(qualifier), inner.resolveStar(qualifier)) match {
        case (None, None) =>
          None
        case (leftAttrs, rightAttrs) =>
          Some(leftAttrs.getOrElse(Nil) ++ rightAttrs.getOrElse(Nil))
      }

    def lookup(ref: ResolvedAttribute): Option[Binding] = {
      (outer.lookup(ref), inner.lookup(ref)) match {
        case (Some(b), None) =>
          val b1 = b.copy(path = First :: b.path)
          Some(b1)
        case (None, Some(b)) =>
          val b1 = b.copy(path = Second :: b.path)
          Some(b1)
        case (Some(_), Some(_)) =>
          throw SqlException(s"Ambiguous reference to $ref")
        case _ => None
      }
    }
  }

  case class AliasContext(parent: Context, alias: String) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] =
      ref.table match {
        case None =>
          parent.resolveColumn(ref)
        case Some(`alias`) =>
          parent.resolveColumn(UnresolvedAttribute(None, ref.name))
        case _ =>
          None
      }

    def resolveStar(qualifier: Option[String]) = qualifier match {
      case None | Some(`alias`) =>
        parent.resolveStar(None)
      case _ =>
        parent.resolveStar(qualifier)
    }

    def lookup(resolved: ResolvedAttribute): Option[Binding] =
      parent.lookup(resolved)
  }

  case class ProjectContext(parent: Context, columns: List[ProjectionColumn]) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      val qualifier = ref.table
      val name = ref.name

      @tailrec
      def matchColumn(expr1: Expression): Boolean = expr1 match {
        case ResolvedProjectedAttribute(_, Some(name1), _) if name == name1 && qualifier.isEmpty =>
          true
        case ResolvedProjectedAttribute(parentExpr, _, _) =>
          matchColumn(parentExpr)
        case tableAttribute: ResolvedTableAttribute =>
          tableAttribute.name == name && qualifier.fold(true)(_ == tableAttribute.table.name)
        case _ => false
      }

      columns.indexWhere {
        case ProjectionColumn(expr, alias) =>
          (alias == Some(name) && qualifier.isEmpty) || matchColumn(expr)
      } match {
        case -1 =>
          None
        case i =>
          val savedScope = currScope
          currScope = scope.copy(ctx = parent)
          val expr = columns(i).expr
          val attr = ResolvedProjectedAttribute(expr, Some(name), i)
          currScope = savedScope
          Some(attr)
      }
    }

    def resolveStar(qualifier: Option[String]) =
      if (qualifier.isDefined)
        parent.resolveStar(qualifier)
      else
        Some(columns)

    def lookup(resolved: ResolvedAttribute): Option[Binding] = resolved match {
      case ResolvedProjectedAttribute(_, _, i) =>
        Some(Binding(scope.name, List(Index(i))))
      case _ =>
        None
    }
  }

  case class AggregateContext(parent: Context, groupedBy: List[Expression], aggregates: List[AggregateExpr]) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = None

    override def resolveAggregate(agg: AggregateExpr): Option[ResolvedAttribute] =
      aggregates.indexOf(agg) match {
        case -1 =>
          None
        case i =>
          Some(ResolvedProjectedAttribute(agg, None, i))
      }

    // should this throw an exception instead?
    def resolveStar(qualifier: Option[String]) = None

    override def lookup(resolved: ResolvedAttribute): Option[Binding] =
      groupedBy.indexOf(resolved) match {
        case -1 =>
          resolved match {
            case ResolvedProjectedAttribute(agg: AggregateExpr, _, i) =>
              val path = if (groupedBy.nonEmpty) List(Index(1), Index(i)) else List(Index(i))
              Some(Binding(scope.name, path))
            case _ => None
          }
        case i =>
          Some(Binding(scope.name, List(Index(0), Index(i))))
      }
  }

  def buildContext(op: Operator): Context = {
    op match {
      case Join(left, right, _, _) => JoinContext(buildContext(left), buildContext(right))
      case Scan(t, id) => TableContext(table(t), id)
      case OrderBy(p, by) => buildContext(p)
      case GroupBy(p, by) => buildContext(p) // FIXME remove
      case Filter(p, predicate) => buildContext(p)
      case Project(p, columns) =>
        val ctx1 = buildContext(p)
        val columns1 = columns.flatMap {
          case col: ProjectionColumn =>
            List(col)
          case star @ UnresolvedStar(qualifier) =>
            ctx1.resolveStar(qualifier).getOrElse {
              throw new SqlException(s"Can't resolve $star")
            }
        }
        ProjectContext(ctx1, columns1)
      case Aggregate(p, groupedBy, aggregates) => AggregateContext(buildContext(p), groupedBy, aggregates)
      case TableAlias(p, a) => AliasContext(buildContext(p), a)
      case SubSelect(p) => buildContext(p)
      case _ => throw new NotImplementedError(s"buildContext($op)")
    }
  }

  // op is unused here, but may be used in overrides (some DBs use + for string concatenation)
  def commonArithmeticType(op: ArithOp, left: ColumnType, right: ColumnType): ColumnType = {
    if (left == right)
      left
    else if (left == DoubleType || right == DoubleType)
      DoubleType
    else if (left == BigIntType || right == BigIntType)
      BigIntType
    else if (left == IntType || right == IntType)
      IntType
    else if (left.isInstanceOf[StringType] || right.isInstanceOf[StringType] || left == AnyType || right == AnyType)
      AnyNumberType
    else throw SqlException("Incompatible types " + left.sqlName + " and " + right.sqlName)
  }

  def getExprType(expr: Expression): ColumnType = {
    expr match {
      case BinOpExpr(op, l, r) =>
        op match {
          case _: LogicOp | _: ComparisonOp =>
            BoolType
          case op: ArithOp =>
            commonArithmeticType(op, getExprType(l), getExprType(r))
          case Concat => BasicStringType
        }
      case LikeExpr(l, r, escape) => BoolType
      case InExpr(l, r) => BoolType
      case ExistsExpr(_) => BoolType
      case NegExpr(opd) => getExprType(opd)
      case NotExpr(_) => BoolType
      case AggregateExpr(Count, _, _) => IntType
      case AggregateExpr(Avg, _, _) => DoubleType
      case AggregateExpr(Sum | Max | Min, _, opd) => getExprType(opd)
      case SubstrExpr(str, from, len) => BasicStringType
      case CaseWhenExpr(list) => getExprType(list(1))
      case Literal(v, t) => t
      case CastExpr(e, t) => t
      case SelectExpr(s) => DoubleType
      case FuncExpr(name, args) => funcType(name, args)
      case Parameter() => AnyType
      case tableAttribute: ResolvedTableAttribute => tableAttribute.sqlType
      case ResolvedProjectedAttribute(parent, _, _) =>
        getExprType(parent)
      case c: UnresolvedAttribute =>
        throw new SqlException("getExprType is only called for resolved expressions")
      case _ => throw new NotImplementedError(s"getExprType($expr)")
    }
  }

  /** Returns the type of `FuncExpr(name, args)`. Override if the type depends on args, use
    * `registerFunctionType` otherwise. Default is `DoubleType`
    */
  def funcType(name: String, args: List[Expression]) = name match {
    case "abs" | "trunc" | "truncate" | "round" | "power" | "mod" | "sign" | "ceiling" | "ceil" | "floor" | "nullif" =>
      getExprType(args.head)
    case "coalesce" =>
      // TODO behavior may be wrong in some cases, but we don't support nulls yet anyway
      args.map(getExprType).reduce(commonArithmeticType(Minus, _, _))
    case _ =>
      funcTypes.getOrElse(name,
        throw new IllegalArgumentException(s"Unknown return type for $name(${args.mkString(", ")}). Override `SqlCompiler.funcType` or call `registerFunctionType` if the type doesn't depend on arguments"))
  }

  def registerFunctionType(name: String, tpe: ColumnType) =
    funcTypes.update(name, tpe)

  private val funcTypes = collection.mutable.Map.empty[String, ColumnType]

  // https://en.wikibooks.org/wiki/SQL_Dialects_Reference
  Seq(
    IntType -> Seq(
      "length", "bit_length", "char_length", "octet_length", "width_bucket", "ascii", "instr"
    ),
    DoubleType -> Seq(
      "asin", "acos", "atan", "atan2", "sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "atanh",
      "sqrt", "exp", "ln", "log", "log10", "rand"
    ),
    BasicStringType -> Seq(
      "chr", "char", "concat", "hex", "lower", "upper", "lcase", "ucase", "lpad", "rpad",
      "trim", "ltrim", "rtrim", "left", "right", "repeat", "reverse", "space", "substring", "substr",
      "replace", "initcap", "translate", "quote", "soundex", "md5", "sha1"
    ),
    DateType -> Seq("current_date", "date"),
    TimeType -> Seq("current_time", "time"),
    TimestampType -> Seq("current_timestamp", "now")
  ).foreach {
    case (tpe, funs) => funs.foreach(registerFunctionType(_, tpe))
  }

  def containsAggregates(column: ProjectionColumn): Boolean =
    containsAggregates(column.expr)

  def containsAggregates(expr: Expression): Boolean =
    expr match {
      case _: AggregateExpr =>
        true
      case BinOpExpr(_, l, r) =>
        containsAggregates(l) || containsAggregates(r)
      case NegExpr(opd) =>
        containsAggregates(opd)
      case NotExpr(opd) =>
        containsAggregates(opd)
      case _ => false
    }

  def depends(on: Operator, subquery: Operator): Boolean = subquery match {
    case Join(left, right, _, _) => depends(on, left) || depends(on, right)
    case Scan(t, _) => false
    case OrderBy(p, by) => depends(on, p) || depends(on, by.map(_.expr))
    case Aggregate(p, groupedBy, columns) => depends(on, p) || depends(on, groupedBy) || depends(on, columns)
    case Filter(p, predicate) => depends(on, p) || depends(on, predicate)
    case Project(p, columns) => depends(on, p) || depends(on, columns.map {
      case col: ProjectionColumn => col.expr
      case _: UnresolvedStar => throw new SqlException(s"$subquery has unresolved expression")
    })
    case TableAlias(t, a) => depends(on, t)
    case SubSelect(p) => depends(on, p)
    case _ => false
  }

  def depends(op: Operator, list: ExprList): Boolean = list.exists(e => depends(op, e))

  def depends(op: Operator, expr: Expression): Boolean = {
    expr match {
      case BinOpExpr(_, l, r) => depends(op, l) || depends(op, r)
      case ExistsExpr(q) => depends(op, q)
      case LikeExpr(l, r, escape) =>
        depends(op, l) || depends(op, r) || escape.exists(depends(op, _))
      case NegExpr(opd) => depends(op, opd)
      case NotExpr(opd) => depends(op, opd)
      case _: Literal | _: Parameter => false
      case CastExpr(exp, typ) => depends(op, exp)
      case resolved: ResolvedAttribute => buildContext(op).lookup(resolved).isDefined
      case SelectExpr(s) => depends(op, s)
      case AggregateExpr(_, _, opd) => depends(op, opd)
      case SubstrExpr(str, from, len) => depends(op, str) || depends(op, from) || depends(op, len)
      case CaseWhenExpr(list) => depends(op, list)
      case InListExpr(sel, lst) => depends(op, sel) || depends(op, lst)
      case InExpr(sel, query) => depends(op, sel) || depends(op, query)
      case FuncExpr(name, args) => depends(op, args)
      case _ =>
        throw new SqlException(s"Can't check if $op depends on $expr")
    }
  }
}
