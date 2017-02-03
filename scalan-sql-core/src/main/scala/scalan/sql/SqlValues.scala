package scalan.sql

import scalan.Scalan

trait SqlValues extends Scalan {
  trait SqlValue
  implicit val SqlValueElem: Elem[SqlValue] = new BaseElem[SqlValue](null)

  implicit object SqlValueNumeric extends Numeric[SqlValue] {
    def compare(x: SqlValue, y: SqlValue): Int = ???
    def plus(x: SqlValue, y: SqlValue): SqlValue = ???
    def minus(x: SqlValue, y: SqlValue): SqlValue = ???
    def times(x: SqlValue, y: SqlValue): SqlValue = ???
    def negate(x: SqlValue): SqlValue = ???
    def fromInt(x: Int): SqlValue = ???
    def toInt(x: SqlValue): Int = ???
    def toLong(x: SqlValue): Long = ???
    def toFloat(x: SqlValue): Float = ???
    def toDouble(x: SqlValue): Double = ???
  }

  case class FromSqlValue[A](override val eResult: Elem[A]) extends UnOp[SqlValue, A]("FromSqlValue", _ => ???)(eResult)
  val ToSqlValue = new UnOp[Any, SqlValue]("ToSqlValue", _ => ???)

  implicit class SqlValueOps(x: Rep[SqlValue]) {
    def to[A: Elem] = FromSqlValue(element[A])(x)
  }
}
