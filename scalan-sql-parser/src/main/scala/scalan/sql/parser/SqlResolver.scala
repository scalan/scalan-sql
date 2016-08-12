package scalan.sql.parser

import scalan.sql.parser.SqlAST._

class SqlResolver(val schema: Schema) {
  def table(name: String) = schema.table(name)
  def indices(tableName: String) = schema.indices(tableName)

  case class Scope(var ctx: Context, outer: Option[Scope], nesting: Int, name: String) {
    def resolve(attribute: UnresolvedAttribute): Option[ResolvedAttribute] =
      ctx.resolve(attribute).orElse(outer.flatMap(_.resolve(attribute)))
    def lookup(attribute: ResolvedAttribute): Option[Binding] =
      ctx.lookup(attribute).orElse(outer.flatMap(_.lookup(attribute)))
  }

  var currScope: Scope = Scope(GlobalContext, None, 0, "scalan")

  def withContext[A](op: Operator)(block: => A) = {
    currScope = Scope(currScope.ctx, Some(currScope), currScope.nesting + 1, s"r${currScope.nesting}")
    currScope.ctx = buildContext(op)
    val result = block
    currScope = currScope.outer.get
    result
  }

  def resolveOperator(op: Operator): Operator = op match {
    case Scan(tableName) =>
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
    case Project(parent, columns) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        val columns1 = columns.map(col => col.copy(expr = resolveExpr(col.expr)))
        Project(parent1, columns1)
      }
    case Filter(parent, predicate) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        Filter(parent1, resolveExpr(predicate))
      }
    case GroupBy(parent, columns) =>
      val parent1 = resolveOperator(parent)
      withContext(parent1) {
        GroupBy(parent1, columns.map(resolveExpr))
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
  }

  def resolveExpr(expr: Expression): Expression = expr match {
    case SelectExpr(op) =>
      SelectExpr(resolveOperator(op))
    case BinOpExpr(op, left, right) =>
      BinOpExpr(op, resolveExpr(left), resolveExpr(right))
    case AggregateExpr(op, distinct, value) =>
      AggregateExpr(op, distinct, resolveExpr(value))
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
    case attribute: UnresolvedAttribute =>
      resolveAttribute(attribute)
  }

  def resolveAttribute(attribute: UnresolvedAttribute) =
    currScope.resolve(attribute).getOrElse {
      throw new SqlException(s"Failed to resolve $attribute in scope $currScope")
    }

  def lookup(attribute: ResolvedAttribute): Binding =
    currScope.lookup(attribute).getOrElse {
      throw new SqlException(s"Failed to lookup $attribute in scope $currScope")
    }

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
      case Scan(t) => Set(table(t))
      case OrderBy(p, by) => tables(p)
      case GroupBy(p, by) => tables(p)
      case Filter(p, predicate) => tables(p) ++ tablesInNestedSelects(predicate)
      case Project(p, columns) => tables(p)
      case TableAlias(t, a) => tables(t)
      case SubSelect(p) => tables(p)
      case _ => throw new NotImplementedError(s"tables($op)")
    }
  }

  def indexToPath(i: Int, n: Int) = {
    if (n > 1 && (n <= 8 || i < 7)) List("_" + (i + 1))
    else {
      val path = List.fill(i)("tail")
      if (i == n - 1) path else path :+ "head"
    }
  }

  case class Binding(scope: String, path: List[String], attribute: ResolvedAttribute)

  abstract class Context {
    def resolve(ref: UnresolvedAttribute): Option[ResolvedAttribute]
    def lookup(resolved: ResolvedAttribute): Option[Binding]

    val scope = currScope
  }

  case object GlobalContext extends Context {
    def resolve(ref: UnresolvedAttribute): Option[ResolvedAttribute] = None
    def lookup(resolved: ResolvedAttribute): Option[Binding] = None
  }

  case class TableContext(table: Table) extends Context {
    def resolve(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      val qualifier = ref.table
      if (qualifier.isEmpty || qualifier == Some(table.name)) {
        table.columns.indexWhere(c => c.name == ref.name) match {
          case -1 => None
          case i =>
            Some(ResolvedTableAttribute(table, i))
        }
      } else
        None
    }

    def lookup(resolved: ResolvedAttribute): Option[Binding] = resolved match {
      case ResolvedTableAttribute(`table`, index) =>
        Some(Binding(scope.name, List(resolved.name), resolved))
      case ResolvedProjectedAttribute(parent: ResolvedAttribute, _, _, _) =>
        lookup(parent)
      case _ => None
    }

    override def toString: String = s"TableContext(${table.name})"
  }

  case class JoinContext(outer: Context, inner: Context) extends Context {
    def resolve(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      (outer.resolve(ref), inner.resolve(ref)) match {
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
          val b1 = b.copy(path = "head" :: b.path)
          Some(b1)
        case (None, Some(b)) =>
          val b1 = b.copy(path = "tail" :: b.path)
          Some(b1)
        case (Some(_), Some(_)) =>
          throw SqlException(s"Ambiguous reference to $ref")
        case _ => None
      }
    }
  }

  case class AliasContext(parent: Context, alias: String) extends Context {
    def resolve(ref: UnresolvedAttribute): Option[ResolvedAttribute] =
      ref.table match {
        case None =>
          parent.resolve(ref)
        case Some(`alias`) =>
          parent.resolve(UnresolvedAttribute(None, ref.name))
        case _ =>
          None
      }

    def lookup(resolved: ResolvedAttribute): Option[Binding] =
      parent.lookup(resolved)
  }

  case class ProjectContext(parent: Context, columns: List[ProjectionColumn]) extends Context {
    def resolve(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
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
        Some(Binding(scope.name, indexToPath(i, columns.length), resolved))
      case _ =>
        None
    }
  }

  def buildContext(op: Operator): Context = {
    op match {
      case Join(outer, inner, _, _) => JoinContext(buildContext(outer), buildContext(inner))
      case Scan(t) => TableContext(table(t))
      case OrderBy(p, by) => buildContext(p)
      case GroupBy(p, by) => buildContext(p)
      case Filter(p, predicate) => buildContext(p)
      case Project(p, columns) => ProjectContext(buildContext(p), columns)
      case TableAlias(p, a) => AliasContext(buildContext(p), a)
      case SubSelect(p) => buildContext(p)
      case _ => throw new NotImplementedError(s"tables($op)")
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
      case c: UnresolvedAttribute => throw new IllegalArgumentException(s"getExprType called for an expression with unresolved column $c")
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

  def isAggregate(column: ProjectionColumn): Boolean =
    isAggregate(column.expr)

  def isAggregate(expr: Expression): Boolean =
    expr match {
      case _: AggregateExpr =>
        true
      case BinOpExpr(_, l, r) =>
        isAggregate(l) || isAggregate(r)
      case NegExpr(opd) =>
        isAggregate(opd)
      case NotExpr(opd) =>
        isAggregate(opd)
      case _ => false
    }

  def isGrandAggregate(columns: List[ProjectionColumn]): Boolean =
    columns.forall(isAggregate)

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
    case Scan(t) => false
    case OrderBy(p, by) => depends(on, p) || using(on, by.map(_.expr))
    case GroupBy(p, by) => depends(on, p) || using(on, by)
    case Filter(p, predicate) => depends(on, p) || using(on, predicate)
    case Project(p, columns) => depends(on, p) || using(on, columns.map(_.expr))
    case TableAlias(t, a) => depends(on, t)
    case SubSelect(p) => depends(on, p)
    case _ => false
  }

  def using(op: Operator, list: ExprList): Boolean = list.exists(e => using(op, e))

  def using(op: Operator, predicate: Expression): Boolean = {
    predicate match {
      case attribute: ResolvedAttribute =>
        buildContext(op).lookup(attribute).isDefined
      case ref: UnresolvedAttribute =>
        throw new IllegalArgumentException("using called on an unresolved expression")
      case BinOpExpr(_, l, r) => using(op, l) || using(op, r)
      case ExistsExpr(q) => depends(op, q)
      case LikeExpr(l, r, escape) =>
        using(op, l) || using(op, r) || escape.exists(using(op, _))
      case NegExpr(opd) => using(op, opd)
      case NotExpr(opd) => using(op, opd)
      case Literal(v, t) => false
      case CastExpr(exp, typ) => using(op, exp)
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
