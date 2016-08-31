package scalan.sql

import java.lang.reflect.Method

import scalan._
import scalan.sql.parser.SqlAST.{Index, SortDirection, Table}

trait Scannables extends ScalanDsl {
  self: ScannablesDsl with ScalanSql =>

  type RScannable[A] = Rep[Scannable[A]]

  trait Scannable[Row] extends Def[Scannable[Row]] {
    def eRow: Elem[Row]

    def fullScan(direction: SortDirection): RIter[Row] = delayInvoke
  }

  abstract class TableScannable[Row](val table: Rep[Table], val scanId: Rep[Int], val fakeDep: Rep[Int])(implicit val eRow: Elem[Row]) extends Scannable[Row]

  abstract class IndexScannable[Row](val table: Rep[Table], val index: Rep[Index], val scanId: Rep[Int], val fakeDep: Rep[Int])(implicit val eRow: Elem[Row]) extends Scannable[Row] {
    def search[Key](numColumns: Int, lowerBound: Option[Bound[Key]], upperBound: Option[Bound[Key]], direction: SortDirection): RIter[Row] = delayInvoke
  }
}

trait ScannablesDsl extends impl.ScannablesAbs { self: ScalanSql =>
  implicit def ScannableElemExtensions[A](ie: Elem[Scannable[A]]) = ie.asInstanceOf[ScannableElem[A, Scannable[A]]]

  case class Bound[Key](value: Rep[Key], isInclusive: Boolean)
}

trait ScannablesDslStd extends impl.ScannablesStd { self: ScalanSqlStd =>
}

trait ScannablesDslExp extends impl.ScannablesExp { self: ScalanSqlExp =>
  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
    case elem: ScannableElem[_, _] =>
      m.getName match {
        case "fullScan" | "search" =>
          iterElement(elem.eRow)
        case _ => super.getResultElem(receiver, m, args)
      }
    case _ =>
      super.getResultElem(receiver, m, args)
  }

  override def formatConst(x: Any) = x match {
    case x: Table => s"Table ${x.name}"
    case x: Index => s"Index ${x.name} ON ${x.tableName}"
    case _ => super.formatConst(x)
  }
}
