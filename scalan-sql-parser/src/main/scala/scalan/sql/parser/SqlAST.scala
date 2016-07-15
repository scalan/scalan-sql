package scalan.sql.parser

import scala.reflect.{ClassTag, classTag}
import scala.util.parsing.input.Positional

object SqlAST {

  abstract sealed class Statement

  case class SqlException(msg: String) extends Exception(msg)

  type Script = List[Statement]
  type Schema = List[Column]
  type ColumnList = List[String]
  type ExprList = List[Expression]

  case class Table(name: String, schema: Schema, constraints: List[TableConstraint], withoutRowId: Boolean) {
    lazy val primaryKey: List[String] = {
      constraints.collectFirst {
        case PrimaryKeyT(columns, _) => columns.map(_.name)
      }.getOrElse {
        schema.filter { _.constraints.exists(_.isInstanceOf[PrimaryKeyC]) } match {
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

  case class Column(name: String, ctype: ColumnType, constraints: List[ColumnConstraint])

  trait ColumnType {
    def sqlName: String
    def scalaName: String
  }

  abstract class SimpleColumnType[A](val sqlName: String)(implicit tag: ClassTag[A]) extends ColumnType {
    val scalaName = tag.toString
  }

  case object IntType extends SimpleColumnType[Int]("integer")
  case object BigIntType extends SimpleColumnType[Long]("bigint")
  case object SmallIntType extends SimpleColumnType[Short]("smallint")
  case object TinyIntType extends SimpleColumnType[Short]("tinyint")

  case object FloatType extends SimpleColumnType[Float]("float")
  case object DoubleType extends SimpleColumnType[Double]("double")
  case class DecimalType(totalDigits: Option[Int], fractionalDigits: Option[Int]) extends ColumnType {
    def sqlName = "decimal"
    val scalaName = classTag[BigDecimal].toString
  }

  case object BoolType extends SimpleColumnType[Boolean]("bool")

  case class StringType(fixed: Boolean, length: Option[Int]) extends SimpleColumnType[String](if (fixed) "char" else "varchar")
  val BasicStringType = StringType(false, None)

  case object BlobType extends SimpleColumnType[Array[Byte]]("blob")

  case object DateType extends SimpleColumnType[java.sql.Date]("date")
  case object TimeType extends SimpleColumnType[java.sql.Time]("time")
  case object TimestampType extends SimpleColumnType[java.sql.Timestamp]("timestamp")

  case class EnumType(values: List[String]) extends ColumnType {
    def sqlName = "enum"
    val scalaName = classTag[String].toString
  }

  sealed trait TableConstraint
  case class PrimaryKeyT(columns: List[IndexedColumn], onConflict: OnConflict) extends TableConstraint
  case class UniqueT(columns: List[IndexedColumn], onConflict: OnConflict) extends TableConstraint
  case class ForeignKeyT(parent: Table, columnNames: List[(String, String)]) extends TableConstraint

  // TODO first parameter should be expr: Expression, but name resolution inside table defs would need to be
  // fixed first
  case class IndexedColumn(name: String, collationSequence: String, direction: SortDirection)

  sealed trait ColumnConstraint
  case class PrimaryKeyC(direction: SortDirection, onConflict: OnConflict, isAutoIncrement: Boolean) extends ColumnConstraint
  case class NotNull(onConflict: OnConflict) extends ColumnConstraint
  case class UniqueC(onConflict: OnConflict) extends ColumnConstraint
  case class Default(expr: Expression) extends ColumnConstraint
  case class ForeignKeyC(parent: Table, parentKey: String) extends ColumnConstraint
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

  abstract sealed class Operator

  case class Scan(table: Table) extends Operator

  case class Distinct(table: Operator) extends Operator
  case class Union(left: Operator, right: Operator) extends Operator
  case class Except(left: Operator, right: Operator) extends Operator
  case class Intersect(left: Operator, right: Operator) extends Operator

  case class TableAlias(table: Operator, alias: String) extends Operator

  case class ProjectionColumn(expr: Expression, alias: Option[String])
  case class Project(parent: Operator, columns: List[ProjectionColumn]) extends Operator

  case class Filter(parent: Operator, predicate: Expression) extends Operator

  case class GroupBy(parent: Operator, columns: ExprList) extends Operator

  abstract sealed class SortDirection
  case object Ascending extends SortDirection
  case object Descending extends SortDirection

  sealed trait NullsOrdering
  case object NullsFirst extends NullsOrdering
  case object NullsLast extends NullsOrdering
  case object NullsOrderingUnspecified extends NullsOrdering

  case class SortSpec(expr: Expression, direction: SortDirection, nulls: NullsOrdering)

  case class OrderBy(parent: Operator, columns: List[SortSpec]) extends Operator

  case class Limit(parent: Operator, limit: Expression) extends Operator

  case class SubSelect(parent: Operator) extends Operator

  sealed trait JoinSpec
  case class On(condition: Expression) extends JoinSpec
  case class Using(columns: ColumnList) extends JoinSpec
  case object Natural extends JoinSpec

  case class Join(outer: Operator, inner: Operator, joinType: JoinType, spec: JoinSpec) extends Operator
  case class CrossJoin(outer: Operator, inner: Operator) extends Operator
  case class UnionJoin(outer: Operator, inner: Operator) extends Operator

  case class SelectStmt(operator: Operator) extends Statement

  case class CreateTableStmt(table: Table) extends Statement

  case class CreateIndexStmt(name: String, table: Table, columns: List[IndexedColumn], isUnique: Boolean) extends Statement

  sealed trait Expression

  case class SelectExpr(stmt: SelectStmt) extends Expression

  sealed trait BinOp

  sealed trait ArithOp extends BinOp
  case object Plus extends ArithOp
  case object Minus extends ArithOp
  case object Times extends ArithOp
  case object Divide extends ArithOp
  case object Modulo extends ArithOp

  sealed trait LogicOp extends BinOp
  case object And extends LogicOp
  case object Or extends LogicOp

  sealed trait ComparisonOp extends BinOp
  case object Eq extends ComparisonOp
  case object Greater extends ComparisonOp
  case object GreaterEq extends ComparisonOp
  case object Less extends ComparisonOp
  case object LessEq extends ComparisonOp
  case object Is extends ComparisonOp

  case object Concat extends BinOp

  case class BinOpExpr(op: BinOp, left: Expression, right: Expression) extends Expression

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
  }
  val CountAllExpr = AggregateExpr(Count, false, Literal(1, IntType))

  case class LikeExpr(left: Expression, right: Expression, escape: Option[Expression]) extends Expression

  case class InListExpr(left: Expression, right: ExprList) extends Expression

  case class InExpr(left: Expression, right: SelectStmt) extends Expression

  case class ExistsExpr(query: Expression) extends Expression

  case class NegExpr(opd: Expression) extends Expression

  case class NotExpr(opd: Expression) extends Expression

  case class SubstrExpr(str: Expression, from: Expression, len: Expression) extends Expression

  case class IsNullExpr(opd: Expression) extends Expression

  case class FuncExpr(name: String, params: ExprList) extends Expression

  case class CastExpr(expr: Expression, to: ColumnType) extends Expression

  case class Literal(value: Any, tp: ColumnType) extends Expression with Positional {
    // set in SqlParser.select
    var index: Option[Int] = None
  }

  case object NullLiteral extends Expression

  // FIXME CaseWhenExpr(operand: Option[Expression], cases: List[(Expression, Expression)], default: Option[Expression])
  case class CaseWhenExpr(list: ExprList) extends Expression

  case class ColumnRef(table: Option[String], name: String) extends Expression {
    def asString = (table match {
      case Some(table) => table + "."
      case None => ""
    }) + name
  }

  def Schema(list: Column*): Schema = list.toList

  def Script(stmts: Statement*): Script = stmts.toList

  def ExprList(exprs: Expression*): ExprList = exprs.toList
}
