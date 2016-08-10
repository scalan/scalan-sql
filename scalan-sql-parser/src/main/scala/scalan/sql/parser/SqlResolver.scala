package scalan.sql.parser

import scalan.sql.parser.SqlAST._

class SqlResolver(val schema: Schema) {
  def table(name: String) = schema.table(name)
  def indices(tableName: String) = schema.indices(tableName)

  case class Scope(var ctx: Context, outer: Option[Scope], nesting: Int, name: String) {
    def lookup(col: ColumnRef): Binding = {
      ctx.resolve(col) match {
        case Some(b) => b
        case None => {
          outer match {
            case Some(s: Scope) => s.lookup(col)
            case _ =>
              throw SqlException( s"""Failed to lookup column ${col.asString}""")
          }
        }
      }
    }
  }

  var currScope: Scope = Scope(new GlobalContext, None, 0, "scalan")

  def pushContext(opd: Operator) = {
    currScope = Scope(currScope.ctx, Some(currScope), currScope.nesting + 1, if (currScope.nesting == 0) "r" else "r" + currScope.nesting.toString)
    currScope.ctx = buildContext(opd)
  }

  def popContext() = {
    currScope = currScope.outer.get
  }

  def lookup(col: ColumnRef): Binding = currScope.lookup(col)

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

  case class Binding(scope: String, path: List[String], column: Column)

  abstract class Context {
    def resolve(ref: ColumnRef): Option[Binding]

    val scope = currScope
  }

  class GlobalContext() extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = None
  }

  case class TableContext(table: Table) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = {
      if (ref.table.isEmpty || ref.table == Some(table.name)) {
        val i = table.columns.indexWhere(c => c.name == ref.name)
        if (i >= 0) {
          //val path = indexToPath(i, table.columns.length);
          val path = List(ref.name)
          Some(Binding(scope.name, path, table.columns(i)))
        } else None
      } else None
    }
  }

  case class JoinContext(outer: Context, inner: Context) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = {
      (outer.resolve(ref), inner.resolve(ref)) match {
        case (Some(b), None) =>
          Some(Binding(b.scope, "head" :: b.path, b.column))
        case (None, Some(b)) =>
          Some(Binding(b.scope, "tail" :: b.path, b.column))
        case (Some(_), Some(_)) =>
          throw SqlException(s"""Ambiguous reference to ${ref.asString}""")
        case _ => None
      }
    }
  }

  case class AliasContext(parent: Context, alias: String) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] =
      ref.table match {
        case None =>
          parent.resolve(ref)
        case Some(`alias`) =>
          parent.resolve(ColumnRef(None, ref.name))
        case _ =>
          None
      }
  }

  case class ProjectContext(parent: Context, columns: List[ProjectionColumn]) extends Context {
    def resolve(ref: ColumnRef): Option[Binding] = {
      val i = columns.indexWhere {
        case ProjectionColumn(c, alias) => matchExpr(c, alias, ref)
      }
      if (i >= 0) {
        val saveScope = currScope
        currScope = Scope(parent, scope.outer, scope.nesting, scope.name)
        val cType = getExprType(columns(i).expr)
        currScope = saveScope
        Some(Binding(scope.name, indexToPath(i, columns.length), Column(ref.name, cType, Nil)))
      }
      else None
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
      case c: ColumnRef => lookup(c).column.ctype
      case SelectExpr(s) => DoubleType
      case FuncExpr(name, args) => funcType(name, args)
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

  def matchExpr(col: Expression, alias: Option[String], exp: Expression): Boolean = {
    col == exp || (exp match {
      case ColumnRef(None, name) =>
        alias == Some(name)
      case _ => false
    })
  }

  def ref(e: Expression): ColumnRef = e match {
    case c: ColumnRef => c
    case _ => throw SqlException("Column reference expected")
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
      case BinOpExpr(_, l, r) => using(op, l) || using(op, r)
      case ExistsExpr(q) => depends(op, q)
      case LikeExpr(l, r, escape) =>
        using(op, l) || using(op, r) || escape.exists(using(op, _))
      case NegExpr(opd) => using(op, opd)
      case NotExpr(opd) => using(op, opd)
      case Literal(v, t) => false
      case CastExpr(exp, typ) => using(op, exp)
      case ref: ColumnRef => buildContext(op).resolve(ref).isDefined
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
