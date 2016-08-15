package scalan.sql.parser

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

    def lookup(col: UnresolvedAttribute): Binding = {
      ctx.lookup(col) match {
        case Some(b) => b
        case None => {
          outer match {
            case Some(s: Scope) =>
              s.lookup(col)
            case _ =>
              throw SqlException(
                s"Failed to lookup column $col")
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

  def resolveAggregates(parent: Operator, groupedBy: ExprList, columns: List[ProjectionColumn]): Operator = {
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
      case Literal(v, t) =>
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

    columns.find { c =>
      val expr = c.expr
      !(containsAggregates(expr) || groupedBy.contains(expr))
    } match {
      case Some(projectionColumn) =>
        throw new SqlException(s"Non-aggregate column $projectionColumn in an aggregate query")
      case None =>
        val normalizedColumns = columns.map { col =>
          val normalized = simplifyAgg(col.expr)
          col.copy(expr = normalized)
        }

        val aggregates = normalizedColumns.flatMap { col =>
          extractAggregateExprs(col.expr)
        }.distinct

        val aggregate = Aggregate(parent, groupedBy.map(resolveExpr), aggregates)

        withContext(aggregate) {
          Project(aggregate, normalizedColumns.map(c => c.copy(expr = resolveExpr(c.expr))))
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
      TableAlias(resolveOperator(table), alias)
    case Filter(parent, predicate) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        Filter(parent1, resolveExpr(predicate))
      }
    case Project(parent, columns) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        if (columns.exists(containsAggregates))
          resolveAggregates(parent1, Nil, columns)
        else {
          val columns1 = columns.map(col => col.copy(expr = resolveExpr(col.expr)))
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
    case Join(outer, inner, joinType, spec) =>
      val outer1 = resolveOperator(outer)
      val inner1 = resolveOperator(inner)
      val condition1 = spec match {
        case On(condition) =>
          withContext(outer1) {
            withContext(inner1) {
              resolveExpr(condition)
            }
          }
        case Using(columns) =>
          def resolvedColumns() = columns.map(name => resolveExpr(UnresolvedAttribute(None, name)))
          val cOuter = withContext(outer1) {
            resolvedColumns()
          }
          val cInner = withContext(inner1) {
            resolvedColumns()
          }
          (cOuter, cInner).zipped.map(BinOpExpr(Eq, _, _)).reduce(BinOpExpr(And, _, _))
        case Natural =>
          // requires implementing operatorType, won't support it yet
          throw new NotImplementedError("Natural joins not supported yet")
      }
      Join(outer1, inner1, joinType, On(condition1))
    case CrossJoin(outer, inner) =>
      CrossJoin(resolveOperator(outer), resolveOperator(inner))
    case UnionJoin(outer, inner) =>
      UnionJoin(resolveOperator(outer), resolveOperator(inner))
    case _ =>
      throw new NotImplementedError(s"Can't resolve operator\n$op")
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
    case Literal(_, _) | NullLiteral | _: ResolvedAttribute =>
      expr
    case unresolved: UnresolvedAttribute =>
      currScope.resolve(unresolved)
  }

  // should only be called from resolveExpr?
  def lookup(col: UnresolvedAttribute): Binding = currScope.lookup(col)

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
      case Join(outer, inner, _, _) => tables(outer) ++ tables(inner)
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

  abstract class Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute]
    def resolveAggregate(agg: AggregateExpr): Option[ResolvedAttribute] = None
    def lookup(resolved: ResolvedAttribute): Option[Binding]
    def lookup(unresolved: UnresolvedAttribute): Option[Binding] = resolveColumn(unresolved).flatMap(lookup)

    val scope = currScope
  }

  case object GlobalContext extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = None
    def lookup(resolved: ResolvedAttribute): Option[Binding] = None
  }

  case class TableContext(table: Table, id: Int) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      val qualifier = ref.table
      if (qualifier.isEmpty || qualifier == Some(table.name)) {
        table.columns.indexWhere(c => c.name == ref.name) match {
          case -1 => None
          case i =>
            Some(ResolvedTableAttribute(table, id, i))
        }
      } else
        None
    }

    def lookup(resolved: ResolvedAttribute): Option[Binding] = resolved match {
      case ResolvedTableAttribute(`table`, `id`, index) =>
        Some(Binding(scope.name, List(Field(resolved.name))))
      case ResolvedProjectedAttribute(parent: ResolvedAttribute, _, _, _) =>
        lookup(parent)
      case _ => None
    }

    override def toString: String = s"TableContext(${table.name})"
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

    def lookup(resolved: ResolvedAttribute): Option[Binding] =
      parent.lookup(resolved)
  }

  case class ProjectContext(parent: Context, columns: List[ProjectionColumn]) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      columns.indexWhere {
        case ProjectionColumn(c, alias) => matchExpr(c, alias, ref)
      } match {
        case -1 =>
          None
        case i =>
          val saveScope = currScope
          currScope = scope.copy(ctx = parent)
          val column = columns(i)
          val parentExpr = column.expr
          val cType = getExprType(parentExpr)
          val attr = ResolvedProjectedAttribute(parentExpr, ref.name, i, cType)
          currScope = saveScope
          Some(attr)
      }
    }

    def lookup(resolved: ResolvedAttribute): Option[Binding] = resolved match {
      case ResolvedProjectedAttribute(_, name, i, _) =>
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
          Some(ResolvedProjectedAttribute(agg, agg.toString, i, getExprType(agg)))
      }

    override def lookup(resolved: ResolvedAttribute): Option[Binding] =
      groupedBy.indexOf(resolved) match {
        case -1 =>
          None
        case i =>
          Some(Binding(scope.name, List(Field("key"), Index(i))))
      }
  }

  def buildContext(op: Operator): Context = {
    op match {
      case Join(outer, inner, _, _) => JoinContext(buildContext(outer), buildContext(inner))
      case Scan(t, id) => TableContext(table(t), id)
      case OrderBy(p, by) => buildContext(p)
      case GroupBy(p, by) => buildContext(p) // FIXME remove
      case Filter(p, predicate) => buildContext(p)
      case Project(p, columns) => ProjectContext(buildContext(p), columns)
      case Aggregate(p, groupedBy, aggregates) => AggregateContext(buildContext(p), groupedBy, aggregates)
      case TableAlias(p, a) => AliasContext(buildContext(p), a)
      case SubSelect(p) => buildContext(p)
      case _ => throw new NotImplementedError(s"buildContext($op)")
    }
  }

  def commonType(left: ColumnType, right: ColumnType): ColumnType = {
    if (left == right)
      left
    else if (left == DoubleType || right == DoubleType)
      DoubleType
    else if (left == BigIntType || right == BigIntType)
      BigIntType
    else if (left == IntType || right == IntType)
      IntType
    else if (left.isInstanceOf[StringType] || right.isInstanceOf[StringType])
      BasicStringType
    else throw SqlException("Incompatible types " + left.sqlName + " and " + right.sqlName)
  }

  def getExprType(expr: Expression): ColumnType = {
    expr match {
      case BinOpExpr(op, l, r) =>
        op match {
          case _: LogicOp | _: ComparisonOp =>
            BoolType
          case _: ArithOp =>
            commonType(getExprType(l), getExprType(r))
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
      case attribute: ResolvedAttribute => attribute.sqlType
      case c: UnresolvedAttribute =>
        // TODO throw an exception after SqlBridge works with resolved expressions
        currScope.resolve(c).sqlType
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
      args.map(getExprType).reduce(commonType)
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

  // complex logic, not sure it's correct!
  def matchExpr(col: Expression, alias: Option[String], exp: Expression): Boolean = {
    col == exp || ((col, exp) match {
      case (_, UnresolvedAttribute(None, name)) if alias == Some(name) =>
        true
      case (resolved: ResolvedAttribute, UnresolvedAttribute(None, name)) =>
        resolved.name == name
      case (UnresolvedAttribute(None, name), resolved: ResolvedAttribute) =>
        name == resolved.name
      case _ => false
    })
  }

  def depends(on: Operator, subquery: Operator): Boolean = subquery match {
    case Join(outer, inner, _, _) => depends(on, outer) || depends(on, inner)
    case Scan(t, _) => false
    case OrderBy(p, by) => depends(on, p) || using(on, by.map(_.expr))
    case Aggregate(p, groupedBy, columns) => depends(on, p) || using(on, groupedBy) || using(on, columns)
    case Filter(p, predicate) => depends(on, p) || using(on, predicate)
    case Project(p, columns) => depends(on, p) || using(on, columns.map(_.expr))
    case TableAlias(t, a) => depends(on, t)
    case SubSelect(p) => depends(on, p)
    case _ => false
  }

  def using(op: Operator, list: ExprList): Boolean = list.exists(e => using(op, e))

  def using(op: Operator, predicate: Expression): Boolean = {
    predicate match {
      case BinOpExpr(_, l, r) => using(op, l) || using(op, r)
      case ExistsExpr(q) => depends(op, q)
      case LikeExpr(l, r, escape) =>
        using(op, l) || using(op, r) || escape.exists(using(op, _))
      case NegExpr(opd) => using(op, opd)
      case NotExpr(opd) => using(op, opd)
      case Literal(v, t) => false
      case CastExpr(exp, typ) => using(op, exp)
      case ref: UnresolvedAttribute => buildContext(op).resolveColumn(ref).isDefined
      case SelectExpr(s) => depends(op, s)
      case AggregateExpr(_, _, opd) => using(op, opd)
      case SubstrExpr(str, from, len) => using(op, str) || using(op, from) || using(op, len)
      case CaseWhenExpr(list) => using(op, list)
      case InListExpr(sel, lst) => using(op, sel) || using(op, lst)
      case InExpr(sel, query) => using(op, sel) || depends(op, query)
      case FuncExpr(name, args) => using(op, args)
      case _ => false
    }
  }

  def and(left: Expression, right: Expression) = {
    if (left == Literal(true, BoolType) || right == Literal(false, BoolType))
      right
    else if (right == Literal(true, BoolType) || left == Literal(false, BoolType))
      left
    else
      BinOpExpr(And, left, right)
  }

  // FIXME should probably be done in SqlBridge, and if here, differently
  // e.g. return list of conjuncts instead of a single expression?
  def optimize(op: Operator, predicate: Expression): (Operator, Expression) = op match {
    case Join(outer, inner, joinType, joinSpec) =>
      if (!using(inner, predicate))
        (Join(Filter(outer, predicate), inner, joinType, joinSpec), Literal(true, BoolType))
      else if (!using(outer, predicate))
        (Join(outer, Filter(inner, predicate), joinType, joinSpec), Literal(true, BoolType))
      else
        predicate match {
          case BinOpExpr(And, l, r) =>
            val (jr, cr) = optimize(op, r)
            val (jl, cl) = optimize(jr, l)
            (jl, and(cl, cr))
          case _ => (op, predicate)
        }
    case _ => (op, predicate)
  }

}
