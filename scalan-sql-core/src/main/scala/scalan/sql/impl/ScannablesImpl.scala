package scalan.sql

import java.lang.reflect.Method
import scalan._
import scalan.sql.parser.SqlAST._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ScannablesAbs extends Scalan with Scannables {
  self: ScannablesDsl with ScalanSql =>

  // single proxy for each type family
  implicit def proxyScannable[Row](p: Rep[Scannable[Row]]): Scannable[Row] = {
    proxyOps[Scannable[Row]](p)(scala.reflect.classTag[Scannable[Row]])
  }

  // familyElem
  class ScannableElem[Row, To <: Scannable[Row]](implicit _eRow: Elem[Row])
    extends EntityElem[To] {
    def eRow = _eRow
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("Row" -> eRow)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[Scannable[Row]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Scannable[Row]] => convertScannable(x) }
      tryConvert(element[Scannable[Row]], this, x, conv)
    }

    def convertScannable(x: Rep[Scannable[Row]]): Rep[To] = {
      x.selfType1 match {
        case _: ScannableElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have ScannableElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def scannableElement[Row](implicit eRow: Elem[Row]): Elem[Scannable[Row]] =
    cachedElem[ScannableElem[Row, Scannable[Row]]](eRow)

  implicit case object ScannableCompanionElem extends CompanionElem[ScannableCompanionAbs] {
    lazy val tag = weakTypeTag[ScannableCompanionAbs]
    protected def getDefaultRep = Scannable
  }

  abstract class ScannableCompanionAbs extends CompanionDef[ScannableCompanionAbs] {
    def selfType = ScannableCompanionElem
    override def toString = "Scannable"
  }
  def Scannable: Rep[ScannableCompanionAbs]
  implicit def proxyScannableCompanionAbs(p: Rep[ScannableCompanionAbs]): ScannableCompanionAbs =
    proxyOps[ScannableCompanionAbs](p)

  abstract class AbsTableScannable[Row]
      (table: Rep[Table], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row])
    extends TableScannable[Row](table, scanId, fakeDep) with Def[TableScannable[Row]] {
    lazy val selfType = element[TableScannable[Row]]
  }
  // elem for concrete class
  class TableScannableElem[Row](val iso: Iso[TableScannableData[Row], TableScannable[Row]])(implicit override val eRow: Elem[Row])
    extends ScannableElem[Row, TableScannable[Row]]
    with ConcreteElem[TableScannableData[Row], TableScannable[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(scannableElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertScannable(x: Rep[Scannable[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Scannable to TableScannable: missing fields List(table, scanId, fakeDep)")
    override def getDefaultRep = TableScannable(element[Table].defaultRepValue, 0, 0)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableScannable[Row]]
    }
  }

  // state representation type
  type TableScannableData[Row] = (Table, (Int, Int))

  // 3) Iso for concrete class
  class TableScannableIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[TableScannableData[Row], TableScannable[Row]] with Def[TableScannableIso[Row]] {
    override def from(p: Rep[TableScannable[Row]]) =
      (p.table, p.scanId, p.fakeDep)
    override def to(p: Rep[(Table, (Int, Int))]) = {
      val Pair(table, Pair(scanId, fakeDep)) = p
      TableScannable(table, scanId, fakeDep)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Int], element[Int]))
    lazy val eTo = new TableScannableElem[Row](self)
    lazy val selfType = new TableScannableIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class TableScannableIsoElem[Row](eRow: Elem[Row]) extends Elem[TableScannableIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new TableScannableIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableScannableIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class TableScannableCompanionAbs extends CompanionDef[TableScannableCompanionAbs] {
    def selfType = TableScannableCompanionElem
    override def toString = "TableScannable"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[TableScannableData[Row]])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]] =
      isoTableScannable(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]] =
      mkTableScannable(table, scanId, fakeDep)

    def unapply[Row](p: Rep[Scannable[Row]]) = unmkTableScannable(p)
  }
  lazy val TableScannableRep: Rep[TableScannableCompanionAbs] = new TableScannableCompanionAbs
  lazy val TableScannable: TableScannableCompanionAbs = proxyTableScannableCompanion(TableScannableRep)
  implicit def proxyTableScannableCompanion(p: Rep[TableScannableCompanionAbs]): TableScannableCompanionAbs = {
    proxyOps[TableScannableCompanionAbs](p)
  }

  implicit case object TableScannableCompanionElem extends CompanionElem[TableScannableCompanionAbs] {
    lazy val tag = weakTypeTag[TableScannableCompanionAbs]
    protected def getDefaultRep = TableScannable
  }

  implicit def proxyTableScannable[Row](p: Rep[TableScannable[Row]]): TableScannable[Row] =
    proxyOps[TableScannable[Row]](p)

  implicit class ExtendedTableScannable[Row](p: Rep[TableScannable[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[TableScannableData[Row]] = isoTableScannable(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoTableScannable[Row](implicit eRow: Elem[Row]): Iso[TableScannableData[Row], TableScannable[Row]] =
    reifyObject(new TableScannableIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkTableScannable[Row](table: Rep[Table], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]]
  def unmkTableScannable[Row](p: Rep[Scannable[Row]]): Option[(Rep[Table], Rep[Int], Rep[Int])]

  abstract class AbsIndexScannable[Row]
      (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row])
    extends IndexScannable[Row](table, index, scanId, fakeDep) with Def[IndexScannable[Row]] {
    lazy val selfType = element[IndexScannable[Row]]
  }
  // elem for concrete class
  class IndexScannableElem[Row](val iso: Iso[IndexScannableData[Row], IndexScannable[Row]])(implicit override val eRow: Elem[Row])
    extends ScannableElem[Row, IndexScannable[Row]]
    with ConcreteElem[IndexScannableData[Row], IndexScannable[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(scannableElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertScannable(x: Rep[Scannable[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Scannable to IndexScannable: missing fields List(table, index, scanId, fakeDep)")
    override def getDefaultRep = IndexScannable(element[Table].defaultRepValue, element[Index].defaultRepValue, 0, 0)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexScannable[Row]]
    }
  }

  // state representation type
  type IndexScannableData[Row] = (Table, (Index, (Int, Int)))

  // 3) Iso for concrete class
  class IndexScannableIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[IndexScannableData[Row], IndexScannable[Row]] with Def[IndexScannableIso[Row]] {
    override def from(p: Rep[IndexScannable[Row]]) =
      (p.table, p.index, p.scanId, p.fakeDep)
    override def to(p: Rep[(Table, (Index, (Int, Int)))]) = {
      val Pair(table, Pair(index, Pair(scanId, fakeDep))) = p
      IndexScannable(table, index, scanId, fakeDep)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Index], pairElement(element[Int], element[Int])))
    lazy val eTo = new IndexScannableElem[Row](self)
    lazy val selfType = new IndexScannableIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class IndexScannableIsoElem[Row](eRow: Elem[Row]) extends Elem[IndexScannableIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IndexScannableIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexScannableIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class IndexScannableCompanionAbs extends CompanionDef[IndexScannableCompanionAbs] {
    def selfType = IndexScannableCompanionElem
    override def toString = "IndexScannable"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[IndexScannableData[Row]])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]] =
      isoIndexScannable(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]] =
      mkIndexScannable(table, index, scanId, fakeDep)

    def unapply[Row](p: Rep[Scannable[Row]]) = unmkIndexScannable(p)
  }
  lazy val IndexScannableRep: Rep[IndexScannableCompanionAbs] = new IndexScannableCompanionAbs
  lazy val IndexScannable: IndexScannableCompanionAbs = proxyIndexScannableCompanion(IndexScannableRep)
  implicit def proxyIndexScannableCompanion(p: Rep[IndexScannableCompanionAbs]): IndexScannableCompanionAbs = {
    proxyOps[IndexScannableCompanionAbs](p)
  }

  implicit case object IndexScannableCompanionElem extends CompanionElem[IndexScannableCompanionAbs] {
    lazy val tag = weakTypeTag[IndexScannableCompanionAbs]
    protected def getDefaultRep = IndexScannable
  }

  implicit def proxyIndexScannable[Row](p: Rep[IndexScannable[Row]]): IndexScannable[Row] =
    proxyOps[IndexScannable[Row]](p)

  implicit class ExtendedIndexScannable[Row](p: Rep[IndexScannable[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[IndexScannableData[Row]] = isoIndexScannable(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexScannable[Row](implicit eRow: Elem[Row]): Iso[IndexScannableData[Row], IndexScannable[Row]] =
    reifyObject(new IndexScannableIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkIndexScannable[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]]
  def unmkIndexScannable[Row](p: Rep[Scannable[Row]]): Option[(Rep[Table], Rep[Index], Rep[Int], Rep[Int])]

  registerModule(Scannables_Module)
}

// Std -----------------------------------
trait ScannablesStd extends ScalanStd with ScannablesDsl {
  self: ScannablesDsl with ScalanSqlStd =>

  lazy val Scannable: Rep[ScannableCompanionAbs] = new ScannableCompanionAbs {
  }

  case class StdTableScannable[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val fakeDep: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsTableScannable[Row](table, scanId, fakeDep) {
  }

  def mkTableScannable[Row]
    (table: Rep[Table], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]] =
    new StdTableScannable[Row](table, scanId, fakeDep)
  def unmkTableScannable[Row](p: Rep[Scannable[Row]]) = p match {
    case p: TableScannable[Row] @unchecked =>
      Some((p.table, p.scanId, p.fakeDep))
    case _ => None
  }

  case class StdIndexScannable[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int], override val fakeDep: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsIndexScannable[Row](table, index, scanId, fakeDep) {
  }

  def mkIndexScannable[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]] =
    new StdIndexScannable[Row](table, index, scanId, fakeDep)
  def unmkIndexScannable[Row](p: Rep[Scannable[Row]]) = p match {
    case p: IndexScannable[Row] @unchecked =>
      Some((p.table, p.index, p.scanId, p.fakeDep))
    case _ => None
  }
}

// Exp -----------------------------------
trait ScannablesExp extends ScalanExp with ScannablesDsl {
  self: ScannablesDsl with ScalanSqlExp =>

  lazy val Scannable: Rep[ScannableCompanionAbs] = new ScannableCompanionAbs {
  }

  case class ExpTableScannable[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val fakeDep: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsTableScannable[Row](table, scanId, fakeDep)

  object TableScannableMethods {
  }

  def mkTableScannable[Row]
    (table: Rep[Table], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]] =
    new ExpTableScannable[Row](table, scanId, fakeDep)
  def unmkTableScannable[Row](p: Rep[Scannable[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TableScannableElem[Row] @unchecked =>
      Some((p.asRep[TableScannable[Row]].table, p.asRep[TableScannable[Row]].scanId, p.asRep[TableScannable[Row]].fakeDep))
    case _ =>
      None
  }

  case class ExpIndexScannable[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int], override val fakeDep: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsIndexScannable[Row](table, index, scanId, fakeDep)

  object IndexScannableMethods {
    object search {
      def unapply(d: Def[_]): Option[(Rep[IndexScannable[Row]], SearchBounds, SortDirection) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(bounds, direction, _*), _) if receiver.elem.isInstanceOf[IndexScannableElem[_]] && method.getName == "search" =>
          Some((receiver, bounds, direction)).asInstanceOf[Option[(Rep[IndexScannable[Row]], SearchBounds, SortDirection) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IndexScannable[Row]], SearchBounds, SortDirection) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIndexScannable[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], fakeDep: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]] =
    new ExpIndexScannable[Row](table, index, scanId, fakeDep)
  def unmkIndexScannable[Row](p: Rep[Scannable[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexScannableElem[Row] @unchecked =>
      Some((p.asRep[IndexScannable[Row]].table, p.asRep[IndexScannable[Row]].index, p.asRep[IndexScannable[Row]].scanId, p.asRep[IndexScannable[Row]].fakeDep))
    case _ =>
      None
  }

  object ScannableMethods {
    object fullScan {
      def unapply(d: Def[_]): Option[(Rep[Scannable[Row]], SortDirection) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(direction, _*), _) if receiver.elem.isInstanceOf[ScannableElem[_, _]] && method.getName == "fullScan" =>
          Some((receiver, direction)).asInstanceOf[Option[(Rep[Scannable[Row]], SortDirection) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Scannable[Row]], SortDirection) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Scannables_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAANVWT4hbRRifl02azR/rulpZweK6pipVk8UiFZZS1m4qW9Js2Le1EosyeW+SvnbevNk3k/XFQws97EE9iXgQPBQUL0UQbwqloIKICHr17KlWSg/2pPjNvD9JdjdxXfVgDsPkzTffn9/vNzPftVsoI3z0uLAwxazsEonLpp4vClkyq0w6snfas7uULJH2D0/d8PJX3q6n0FQT7TuPxZKgTZQLJ9WAJ3NT2jWUw8wiQnq+kOjRmo5QsTxKiSUdj1Uc1+1K3KKkUnOEXKihdMuze+voEjJqaMrymOUTScwTFAtBRPR9kqiMnOR/Tv/vrfB+DFZRVVQGqljzsSMhfYgxFdqvEm72mMd6rkT7o9RWuEoLbAok4FDDssupDjNRQ1nH5Z4v46hZiHDes+O/aYbhA5quXcAbuAJROxVT+g7rKGccWxdxh9TBRJmnoQZBaHutx0nkvCCkPRQv4AghYOVZnVi5j1k5waysMCuZxHcwdd7AarHhe0EPhT9jAqGAg4un/8JF7IFUmV1685z1yl2z4KbU5kClktUJ7QNHj4xQiKYHsP1m9R1x58WrR1Mo30R5Ryy2hPSxJQdlEMFVwIx5UuecIIj9DjA4N4pBHWURbLbIJGd5LscMPEVYFoEo6liOVMbqWzGiZwT2WclJbGoE3EjqnR1Rr9bSCUxp4+ZDzxz6pfpyCqWGQ+TApQmHwY+dSpQDB4wp0CP/arxXoolV73UNsxpyQX/MjskgweKJm7/aX8+jc6kEwSjg7kgDF9PPv//5IdL4JIUmm1rjJynuaPoUREtEWE006W0QP/ye3cBUzXakMGuTNu5SGQE7iMgEICLR7MjjyYmCa0HL3ojLL4TKrXuMlE42Sr+Z3757TQnTR8VwJTyvfzhHf/9pf1tqzUqUkQnKGl/ChxHPrKn1Acz1wkwSWA0HJdgDYcv2Dn589NgoZXDS8B0XbqcN8tyXX5y5fb2e0eKYjpB5CdMuCS+GCJg+SCp3Yx4iLTM5LrFsG18EYvj2zNRweJuO9DZtcGAAiBlji9s0ASXGPtNVStxdCBWuTo1mIu5+MIXTwdE4gfQeXK09QG8dv55CmVMo0wZViRrKtLwus+OjDE+AJIF8If5mDKsKji72sRuLPrz4ZpFOQue6LW1tWjCGS9vD2dwG51aW/iUVZhxmk2CMm2W1vncxD0jmf6E2Xe6OalPjsV0oYKDYcfdj3DB8url54PaHr92vH7XJliNdzEvzf+NJU/Pif/tkoS3YQdr/UOBDMk8sQvKGHrO9M6BGHm73UT68z03PJffN3XFevfqW1E+aEQz3VCutC9DDLOjND8O+8gj2lohFsU9s1VARFxq+kJcj7x0/e2rm7Bntu2hro3AleZt2bk9PY76gm6knxzRTYFSquhyaZZgc+erYj5e/+/gj/Sj1EZQo3+dCRnWzslgP+77LEGJuRE1mpAFg99LdD+qHv//sZ/225JWa4IlkSW86+KYME35PPzi0m32OI41AKHOdDlAFZ1NJL8pNjVfUsPknliHSWjQMAAA="
}
}

