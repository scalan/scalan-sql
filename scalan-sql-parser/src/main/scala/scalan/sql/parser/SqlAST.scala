package scalan.sql.parser

import scala.util.parsing.input.Positional

object ID {
  private var i: Int = 0
  def next() = {
    i += 1
    i
  }
}

object SqlAST {

  abstract sealed class Statement

  case class SqlException(msg: String) extends Exception(msg)

  type Script = List[Statement]
  type ColumnList = List[String]
  type ExprList = List[Expression]

  case class Table(name: String, columns: List[Column], constraints: List[TableConstraint], withoutRowId: Boolean) {
    lazy val primaryKey: List[String] = {
      constraints.collectFirst {
        case PrimaryKeyT(columns, _) => columns.map(_.name)
      }.getOrElse {
        columns.filter { _.constraints.exists(_.isInstanceOf[PrimaryKeyC]) } match {
          case Seq(col) => List(col.name)
          case columns =>
            val msg =
              if (columns.isEmpty)
                s"No PRIMARY KEY column in table $name"
              else
                s"Multiple PRIMARY KEY column in table $name: ${columns.map(_.name).mkString(", ")}"
            throw new SqlException(msg)
        }
      }
    }
  }
  case class Index(name: String, tableName: String, columns: List[IndexedColumn], isUnique: Boolean, isPrimaryKey: Boolean)

  case class Schema(tables: Map[String, Table], indicesByName: Map[String, List[Index]]) {
    def table(name: String) =
      tables.getOrElse(name, throw new IllegalArgumentException(s"Table $name not found"))
    def indices(tableName: String) = indicesByName.getOrElse(tableName, Nil)
  }

  case class Column(name: String, ctype: ColumnType, constraints: List[ColumnConstraint])

  trait ColumnType {
    def sqlName: String
    override def toString = sqlName
  }

  abstract class SimpleColumnType(val sqlName: String) extends ColumnType

  case object IntType extends SimpleColumnType("INTEGER")
  case object BigIntType extends SimpleColumnType("BIGINT")
  case object SmallIntType extends SimpleColumnType("SMALLINT")
  case object TinyIntType extends SimpleColumnType("TINYINT")

  case object FloatType extends SimpleColumnType("FLOAT")
  case object DoubleType extends SimpleColumnType("DOUBLE")
  case class DecimalType(totalDigits: Option[Int], fractionalDigits: Option[Int]) extends ColumnType {
    val sqlName = "DECIMAL" + ((totalDigits, fractionalDigits) match {
      case (None, None) => ""
      case (Some(t), None) => s"($t)"
      case (None, Some(f)) => "s(, $f)" // not actually legal
      case (Some(t), Some(f)) => s"($t, $f)"
    })
  }

  case object BoolType extends SimpleColumnType("BOOL")

  case class StringType(fixed: Boolean, length: Option[Int]) extends ColumnType {
    val sqlName = (if (fixed) "CHAR" else "VARCHAR") + length.fold("")(l => s"($l)")
  }
  val BasicStringType = StringType(false, None)

  case object BlobType extends SimpleColumnType("BLOB")

  case object DateType extends SimpleColumnType("DATE")
  case object TimeType extends SimpleColumnType("TIME")
  case object TimestampType extends SimpleColumnType("TIMESTAMP")

  case class EnumType(values: List[String]) extends ColumnType {
    val sqlName = s"ENUM(${values.mkString(", ")})"
  }

  sealed trait TableConstraint
  case class PrimaryKeyT(columns: List[IndexedColumn], onConflict: OnConflict) extends TableConstraint
  case class UniqueT(columns: List[IndexedColumn], onConflict: OnConflict) extends TableConstraint
  case class ForeignKeyT(parent: String, columnNames: List[(String, String)]) extends TableConstraint

  // TODO first parameter should be expr: Expression, but name resolution inside table defs would need to be
  // fixed first
  case class IndexedColumn(name: String, collationSequence: String, direction: SortDirection)

  sealed trait ColumnConstraint
  case class PrimaryKeyC(direction: SortDirection, onConflict: OnConflict, isAutoIncrement: Boolean) extends ColumnConstraint
  case class NotNull(onConflict: OnConflict) extends ColumnConstraint
  case class UniqueC(onConflict: OnConflict) extends ColumnConstraint
  case class Default(expr: Expression) extends ColumnConstraint
  case class ForeignKeyC(parent: String, parentKey: String) extends ColumnConstraint
  case class Collate(collationSequence: String) extends ColumnConstraint

  case class Check(expr: Expression) extends ColumnConstraint with TableConstraint

  sealed trait OnConflict
  object OnConflict {
    case object Abort extends OnConflict
    case object Rollback extends OnConflict
    case object Fail extends OnConflict
    case object Ignore extends OnConflict
    case object Replace extends OnConflict
  }

  sealed abstract class JoinType
  case object Inner extends JoinType
  case object LeftOuter extends JoinType
  case object RightOuter extends JoinType
  case object FullOuter extends JoinType
  case object LeftSemi extends JoinType

  abstract sealed trait Node[T <: Node[T]] {
    def noParentheses = false
  }
  private def p(x: Node[_]) = if (x.noParentheses) x.toString else s"($x)"

  abstract sealed class Operator extends Node[Operator]

  case class Scan(tableName: String, id: Int = ID.next()) extends Operator {
    override def toString = s"$tableName{$id}"
    override def noParentheses = true
  }

  case class Distinct(op: Operator) extends Operator {
    override def toString = s"DISTINCT $op"
  }
  case class Union(left: Operator, right: Operator) extends Operator {
    override def toString = s"${p(left)} UNION ${p(right)}"
  }
  case class Except(left: Operator, right: Operator) extends Operator {
    override def toString = s"${p(left)} UNION ${p(right)}"
  }
  case class Intersect(left: Operator, right: Operator) extends Operator {
    override def toString = s"${p(left)} UNION ${p(right)}"
  }

  case class TableAlias(parent: Operator, alias: String) extends Operator {
    override def toString = s"${p(parent)} AS $alias"
  }

  sealed trait SelectListElement
  case class ProjectionColumn(expr: Expression, alias: Option[String]) extends SelectListElement {
    def mapExpr(f: Expression => Expression) = {
      val expr1 = f(expr)
      copy(expr = expr1)
    }

    override def toString = alias match {
      case None => expr.toString
      case Some(a) => s"$expr AS $a"
    }
  }
  case class UnresolvedStar(qualifier: Option[String]) extends SelectListElement {
    override def toString = qualifier.fold("*")(q => s"$q.*")
  }

  // after resolution, can only have ProjectionColumn
  case class Project(parent: Operator, columns: List[SelectListElement]) extends Operator {
    override def toString = s"$parent\nSELECT ${columns.mkString(", ")}"
  }

  case class Filter(parent: Operator, predicate: Expression) extends Operator {
    override def toString = s"$parent\nWHERE $predicate"
  }

  case class GroupBy(parent: Operator, columns: ExprList) extends Operator {
    override def toString = s"$parent\nGROUP BY ${columns.mkString(", ")}"
  }

  case class Aggregate(parent: Operator, groupedBy: List[Expression], aggregates: List[AggregateExpr]) extends Operator {
    override def toString =
      s"$parent\nAGGREGATE ${aggregates.mkString(", ")}" +
        (if (groupedBy.nonEmpty) s" GROUP BY ${groupedBy.mkString(", ")}" else "")
  }

  abstract sealed class SortDirection
  case object Ascending extends SortDirection
  case object Descending extends SortDirection

  sealed trait NullsOrdering
  case object NullsFirst extends NullsOrdering
  case object NullsLast extends NullsOrdering
  case object NullsOrderingUnspecified extends NullsOrdering

  case class SortSpec(expr: Expression, direction: SortDirection, nulls: NullsOrdering) {
    override def toString = p(expr) + (direction match { case Ascending => " ASC"; case Descending => " DESC" }) + (nulls match {
      case NullsFirst => " NULLS FIRST"
      case NullsLast => " NULLS LAST"
      case NullsOrderingUnspecified => ""
    })
  }

  case class OrderBy(parent: Operator, columns: List[SortSpec]) extends Operator {
    override def toString = s"$parent\nORDER BY ${columns.mkString(", ")}"
  }

  case class Limit(parent: Operator, limit: Expression) extends Operator {
    override def toString = s"$parent\nLIMIT $limit"
  }

  case class SubSelect(parent: Operator) extends Operator {
    override def toString = parent.toString
  }

  sealed trait JoinSpec
  case class On(condition: Expression) extends JoinSpec
  case class Using(columns: ColumnList) extends JoinSpec
  case object Natural extends JoinSpec

  case class Join(left: Operator, right: Operator, joinType: JoinType, spec: JoinSpec) extends Operator {
    override def toString = {
      val (inside1, end) = spec match {
        case On(condition) => ("", s" ON $condition")
        case Using(columns) => ("", s" USING (${columns.mkString(", ")})")
        case Natural => ("NATURAL ", "")
      }
      val inside2 = joinType match {
        case Inner => ""
        case LeftOuter => "LEFT "
        case RightOuter => "RIGHT "
        case FullOuter => "FULL "
        case LeftSemi => "LEFT SEMI "
      }
      s"${p(left)} $inside1${inside2}JOIN ${p(right)}$end"
    }
  }
  case class CrossJoin(outer: Operator, inner: Operator) extends Operator {
    override def toString = s"${p(outer)} CROSS JOIN ${p(inner)}"
  }
  case class UnionJoin(outer: Operator, inner: Operator) extends Operator {
    override def toString = s"${p(outer)} UNION JOIN ${p(inner)}"
  }

  case class SelectStmt(operator: Operator) extends Statement

  case class CreateTableStmt(table: Table) extends Statement

  case class CreateIndexStmt(index: Index) extends Statement

  sealed trait Expression extends Node[Expression]

  case class SelectExpr(op: Operator) extends Expression {
    override def toString = op.toString
  }

  sealed abstract class BinOp(val name: String)

  sealed abstract class ArithOp(name: String) extends BinOp(name)
  case object Plus extends ArithOp("+")
  case object Minus extends ArithOp("-")
  case object Times extends ArithOp("*")
  case object Divide extends ArithOp("/")
  case object Modulo extends ArithOp("%")

  sealed abstract class LogicOp(name: String) extends BinOp(name)
  case object And extends LogicOp("AND")
  case object Or extends LogicOp("OR")

  sealed abstract class ComparisonOp(name: String) extends BinOp(name) {
    def inverse: ComparisonOp

    def inverseIfDescending(direction: SortDirection) = direction match {
      case Ascending => this
      case Descending => inverse
    }
  }
  case object Eq extends ComparisonOp("=") {
    def inverse = Eq
  }
  case object Greater extends ComparisonOp(">") {
    def inverse = Less
  }
  case object GreaterEq extends ComparisonOp(">=") {
    def inverse = LessEq
  }
  case object Less extends ComparisonOp("<") {
    def inverse = Greater
  }
  case object LessEq extends ComparisonOp("<=") {
    def inverse = GreaterEq
  }
  case object Is extends ComparisonOp("IS") {
    def inverse = Is
  }

  case object Concat extends BinOp("||")

  case class BinOpExpr(op: BinOp, left: Expression, right: Expression) extends Expression {
    override def toString = s"${p(left)} ${op.name} ${p(right)}"
  }

  sealed trait AggregateOp
  case object Count extends AggregateOp
  case object Sum extends AggregateOp
  case object Avg extends AggregateOp
  case object Max extends AggregateOp
  case object Min extends AggregateOp
//  case object Every extends AggregateOp
//  case object Any extends AggregateOp
//  case object Some extends AggregateOp

  case class AggregateExpr(op: AggregateOp, distinct: Boolean, value: Expression) extends Expression {
    def isCountAll = this == CountAllExpr

    override def toString = s"${op.toString.toUpperCase}(${if (distinct) "DISTINCT " else ""}$value)"
    override def noParentheses = true
  }
  val CountAllExpr = AggregateExpr(Count, false, Literal(1, IntType))

  case class LikeExpr(left: Expression, right: Expression, escape: Option[Expression]) extends Expression {
    override def toString = s"$left LIKE $right" + escape.fold("")(e => s" ESCAPE $e")
  }

  case class InListExpr(expr: Expression, list: ExprList) extends Expression {
    override def toString = s"$expr IN (${list.mkString(", ")})"
  }

  case class InExpr(left: Expression, subquery: Operator) extends Expression {
    override def toString = s"$left IN $subquery"
  }

  case class ExistsExpr(op: Operator) extends Expression {
    override def toString = s"EXISTS ${p(op)}"
  }

  case class NegExpr(opd: Expression) extends Expression {
    override def toString = s"-${p(opd)}"
  }

  case class NotExpr(opd: Expression) extends Expression {
    override def toString = s"NOT ${p(opd)}"
  }

  case class SubstrExpr(str: Expression, from: Expression, len: Expression) extends Expression {
    override def toString = s"SUBSTR($str, $from, $len)"
    override def noParentheses = true
  }

  case class IsNullExpr(opd: Expression) extends Expression {
    override def toString = s"${p(opd)} IS NULL"
    override def noParentheses = true
  }

  case class FuncExpr(name: String, params: ExprList) extends Expression {
    override def toString = s"$name(${params.mkString(", ")}"
    override def noParentheses = true
  }

  case class CastExpr(expr: Expression, to: ColumnType) extends Expression {
    override def toString = s"${p(expr)} AS $to"
  }

  case class Literal(value: Any, tp: ColumnType) extends Expression with Positional {
    // set in SqlParser.select
    var index: Option[Int] = None
    override def toString = {
      tp match {
        case _: StringType => s"'$value'"
        case DateType => s"DATE'$value'"
        case TimeType => s"TIME'$value'"
        case TimestampType => s"TS'$value'"
        case _ => value.toString
      }
    }
    override def noParentheses = true
  }

  case object NullLiteral extends Expression {
    override def toString = "NULL"
    override def noParentheses = true
  }

  // FIXME CaseWhenExpr(operand: Option[Expression], cases: List[(Expression, Expression)], default: Option[Expression])
  case class CaseWhenExpr(list: ExprList) extends Expression {
    override def toString = {
      "CASE" + list.grouped(2).map {
        // no other list lengths are possible
        case List(cond, res) => s" WHEN $cond THEN $res"
        case List(last) => s" ELSE $last"
      }.mkString
    }
  }

  // We could have Expression[C] and
  // type UnresolvedExpression = Expression[UnresolvedAttribute]
  // type ResolvedExpression = Expression[ResolvedAttribute]
  // to distinguish resolved and unresolved expressions and operators statically
  // TODO: determine whether this is a good idea
  case class UnresolvedAttribute(table: Option[String], name: String) extends Expression {
    override def toString = "[Unresolved]" + table.fold("")(_ + ".") + name
    override def noParentheses = true
  }

  sealed trait ResolvedAttribute extends Expression {
    override def noParentheses = true
  }
  case class ResolvedTableAttribute(table: Table, tableId: Int, index: Int) extends ResolvedAttribute {
    val column = table.columns(index)
    val name = column.name
    val sqlType = column.ctype
    override def toString = s"[Resolved]${table.name}{$tableId}.$name"
  }
  case class ResolvedProjectedAttribute(parent: Expression, alias: Option[String], index: Int) extends ResolvedAttribute {
    override def toString = "[Resolved]" + (alias match {
      case None => p(parent)
      case Some(n) => s"($parent AS $n)"
    })
    // assert(parent.isResolved) uncomment when/if Expression.isResolved is added
  }

  def Script(stmts: Statement*): Script = stmts.toList
}
