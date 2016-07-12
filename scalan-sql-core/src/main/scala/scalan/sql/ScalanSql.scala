package scalan.sql

import java.sql.{Date, Time, Timestamp}

import scalan.{AnalyzingExp, _}
import scala.reflect.runtime.universe._
import scalan.common.Default

/**
  * Use this traits for the top level scalan.sql extensions, customizations and overrides
  */
trait ScalanSql extends ScalanDsl with ItersDsl {
  implicit val DateElement: Elem[Date] = new BaseElem()(weakTypeTag[Date], Default(new Date(0)))
  implicit val DateOrdering: Ordering[Date] = Ordering.by(_.getTime)

  implicit val TimeElement: Elem[Time] = new BaseElem()(weakTypeTag[Time], Default(new Time(0)))
  implicit val TimeOrdering: Ordering[Time] = Ordering.by(_.getTime)

  implicit val TimestampElement: Elem[Timestamp] = new BaseElem()(weakTypeTag[Timestamp], Default(new Timestamp(0)))
  implicit val TimestampOrdering: Ordering[Timestamp] = Ordering.by(_.getTime)

  implicit val BooleanNumeric: Numeric[Boolean] = new Numeric[Boolean] {
    def plus(x: Boolean, y: Boolean): Boolean = ???
    def minus(x: Boolean, y: Boolean): Boolean = ???
    def times(x: Boolean, y: Boolean): Boolean = ???
    def negate(x: Boolean): Boolean = ???
    def fromInt(x: Int): Boolean = x match {
      case 0 => false
      case 1 => true
    }
    def toInt(x: Boolean): Int = if (x) 1 else 0
    def toLong(x: Boolean): Long = toInt(x)
    def toFloat(x: Boolean): Float = toInt(x)
    def toDouble(x: Boolean): Double = toInt(x)
    def compare(x: Boolean, y: Boolean) = x compare y
  }

  val QueryTextKey = MetaKey[String]("queryText")

  // same as ToString, but without rewriting (x: String).toString => x
  // this way it can be used to generate to_lua_string in the end
  def toPlatformString[A](x: Rep[A]): Rep[String]

  val MaterializeKey = MetaKey[Unit]("materialize")
}

trait ScalanSqlStd extends ScalanDslStd with ItersDslStd with ScalanSql
trait ScalanSqlExp extends ScalanDslExp with ItersDslExp with ScalanSql with SqlSlicing {
  def toPlatformString[A](x: Rep[A]): Rep[String] = ToString1[A]()(x)
  case class ToString1[A]() extends UnOp[A, String]("toPlatformString", _.toString)

  // ctx ensures parameters are read inside lambdas only
  case class Parameter[A](index: Int, ctx: Exp[_], value: Any)(implicit val selfType: Elem[A]) extends Def[A]
}
