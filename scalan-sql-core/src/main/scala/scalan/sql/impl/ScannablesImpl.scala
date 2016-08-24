package scalan.sql

import java.lang.reflect.Method
import scalan._
import scalan.sql.parser.SqlAST.{Index, SortDirection, Table}
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
      (table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends TableScannable[Row](table, scanId) with Def[TableScannable[Row]] {
    lazy val selfType = element[TableScannable[Row]]
  }
  // elem for concrete class
  class TableScannableElem[Row](val iso: Iso[TableScannableData[Row], TableScannable[Row]])(implicit override val eRow: Elem[Row])
    extends ScannableElem[Row, TableScannable[Row]]
    with ConcreteElem[TableScannableData[Row], TableScannable[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(scannableElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertScannable(x: Rep[Scannable[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Scannable to TableScannable: missing fields List(table, scanId)")
    override def getDefaultRep = TableScannable(element[Table].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableScannable[Row]]
    }
  }

  // state representation type
  type TableScannableData[Row] = (Table, Int)

  // 3) Iso for concrete class
  class TableScannableIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[TableScannableData[Row], TableScannable[Row]] with Def[TableScannableIso[Row]] {
    override def from(p: Rep[TableScannable[Row]]) =
      (p.table, p.scanId)
    override def to(p: Rep[(Table, Int)]) = {
      val Pair(table, scanId) = p
      TableScannable(table, scanId)
    }
    lazy val eFrom = pairElement(element[Table], element[Int])
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
    def apply[Row](table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]] =
      mkTableScannable(table, scanId)

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
  def mkTableScannable[Row](table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]]
  def unmkTableScannable[Row](p: Rep[Scannable[Row]]): Option[(Rep[Table], Rep[Int])]

  abstract class AbsIndexScannable[Row]
      (table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends IndexScannable[Row](table, index, scanId) with Def[IndexScannable[Row]] {
    lazy val selfType = element[IndexScannable[Row]]
  }
  // elem for concrete class
  class IndexScannableElem[Row](val iso: Iso[IndexScannableData[Row], IndexScannable[Row]])(implicit override val eRow: Elem[Row])
    extends ScannableElem[Row, IndexScannable[Row]]
    with ConcreteElem[IndexScannableData[Row], IndexScannable[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(scannableElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertScannable(x: Rep[Scannable[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Scannable to IndexScannable: missing fields List(table, index, scanId)")
    override def getDefaultRep = IndexScannable(element[Table].defaultRepValue, element[Index].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexScannable[Row]]
    }
  }

  // state representation type
  type IndexScannableData[Row] = (Table, (Index, Int))

  // 3) Iso for concrete class
  class IndexScannableIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[IndexScannableData[Row], IndexScannable[Row]] with Def[IndexScannableIso[Row]] {
    override def from(p: Rep[IndexScannable[Row]]) =
      (p.table, p.index, p.scanId)
    override def to(p: Rep[(Table, (Index, Int))]) = {
      val Pair(table, Pair(index, scanId)) = p
      IndexScannable(table, index, scanId)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Index], element[Int]))
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
    def apply[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]] =
      mkIndexScannable(table, index, scanId)

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
  def mkIndexScannable[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]]
  def unmkIndexScannable[Row](p: Rep[Scannable[Row]]): Option[(Rep[Table], Rep[Index], Rep[Int])]

  registerModule(Scannables_Module)
}

// Std -----------------------------------
trait ScannablesStd extends ScalanStd with ScannablesDsl {
  self: ScannablesDsl with ScalanSqlStd =>

  lazy val Scannable: Rep[ScannableCompanionAbs] = new ScannableCompanionAbs {
  }

  case class StdTableScannable[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsTableScannable[Row](table, scanId) {
  }

  def mkTableScannable[Row]
    (table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]] =
    new StdTableScannable[Row](table, scanId)
  def unmkTableScannable[Row](p: Rep[Scannable[Row]]) = p match {
    case p: TableScannable[Row] @unchecked =>
      Some((p.table, p.scanId))
    case _ => None
  }

  case class StdIndexScannable[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsIndexScannable[Row](table, index, scanId) {
  }

  def mkIndexScannable[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]] =
    new StdIndexScannable[Row](table, index, scanId)
  def unmkIndexScannable[Row](p: Rep[Scannable[Row]]) = p match {
    case p: IndexScannable[Row] @unchecked =>
      Some((p.table, p.index, p.scanId))
    case _ => None
  }
}

// Exp -----------------------------------
trait ScannablesExp extends ScalanExp with ScannablesDsl {
  self: ScannablesDsl with ScalanSqlExp =>

  lazy val Scannable: Rep[ScannableCompanionAbs] = new ScannableCompanionAbs {
  }

  case class ExpTableScannable[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsTableScannable[Row](table, scanId)

  object TableScannableMethods {
  }

  def mkTableScannable[Row]
    (table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableScannable[Row]] =
    new ExpTableScannable[Row](table, scanId)
  def unmkTableScannable[Row](p: Rep[Scannable[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TableScannableElem[Row] @unchecked =>
      Some((p.asRep[TableScannable[Row]].table, p.asRep[TableScannable[Row]].scanId))
    case _ =>
      None
  }

  case class ExpIndexScannable[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsIndexScannable[Row](table, index, scanId)

  object IndexScannableMethods {
    object search {
      def unapply(d: Def[_]): Option[(Rep[IndexScannable[Row]], Int, Option[Bound[Key]], Option[Bound[Key]], SortDirection) forSome {type Row; type Key}] = d match {
        case MethodCall(receiver, method, Seq(numColumns, lowerBound, upperBound, direction, _*), _) if receiver.elem.isInstanceOf[IndexScannableElem[_]] && method.getName == "search" =>
          Some((receiver, numColumns, lowerBound, upperBound, direction)).asInstanceOf[Option[(Rep[IndexScannable[Row]], Int, Option[Bound[Key]], Option[Bound[Key]], SortDirection) forSome {type Row; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IndexScannable[Row]], Int, Option[Bound[Key]], Option[Bound[Key]], SortDirection) forSome {type Row; type Key}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIndexScannable[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexScannable[Row]] =
    new ExpIndexScannable[Row](table, index, scanId)
  def unmkIndexScannable[Row](p: Rep[Scannable[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexScannableElem[Row] @unchecked =>
      Some((p.asRep[IndexScannable[Row]].table, p.asRep[IndexScannable[Row]].index, p.asRep[IndexScannable[Row]].scanId))
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
  val dump = "H4sIAAAAAAAAAL1WT4hbRRj/XnbTbJK1rquVFSyua6pSNVks0sIiZe2msiXNhn1bK7Eok/cm6avz5s2+N1lfRCp46EE9iXgQPBQUL0UQbwqloIKICHr17KlWSg/2pPjNvD9Jdjfruv7JYZjMfO/78/v9Zua7cgOygQ8PBxZhhJddKknZ1PPFQJbMKpeO7J327C6jS7T9w2PXvMIbb9czMNWEfedJsBSwJuSjSTUU6dyUdg3yhFs0kJ4fSHiwpiNULI8xaknH4xXHdbuStBit1JxALtRgvOXZvXW4CEYNpiyPWz6V1DzBSBDQIF6foCojJ/2f1/97K6Ifg1dUFZWBKtZ84khMH2NMRfarVJg97vGeK2F/nNqKUGmhTZGGAmtYdgXTYcZqkHNc4fkyiZrDCOc9O/k7zgkuwHTtAtkgFYzaqZjSd3hHORPEepl0aB1NlPk41hBQ1l7rCRo7LwbSHooXCgBAVp7UiZX7mJVTzMoKs5JJfYcw51WiNhu+F/Yg+hljAKFAF4//hYvEA61yu/TmOeuF22bRzaiPQ5VKTie0Dx09MEIhmh7E9pvVd4Jbz14+moFCEwpOsNgKpE8sOSiDGK4i4dyTOucUQeJ3kMG5UQzqKItos0kmectzBeHoKcZyEolijuVIZazWJmN6RmCfk4ImpkYojLTe2RH1ai2dIIw1rt/3xKFfqs9nIDMcIo8uTTwMfuJUQh4dcK5Aj/2r8U4JY6veKxpmNeTD/pjbIYMUi0eu/2p/PQ/nMimCccDdkYYupo+9//kh2vgkAxNNrfGTjHQ0fQqiJRpYTZjwNqgfrec2CFOzbSnM2bRNukzGwA4iMoaISJgdeTwFVXAtaNkbSfnFSLl1j9PSyUbpN/Pbd68oYfowGe1E5/UP5+jvP+1vS61ZCVmZoqzxpWIY8eya2h/AXG/MpIHVcFCiPRK2bG/jx4eHRilD0IbvuHg7bdCnvvzizM2r9awWx3SMzHOEdWl0McTA9EFSuRvzGGmZyy1i0Enp+AcGqpkxNiU9TlFOScrjVUbdXagN7z8NSarQfjBV7MHRxaJ+7l2t3cNuHL+agewpyLZRGkENsi2vy+3kPOI9Lmkon0nWjGFp4PkjPnET5Ua31yzoJHSuW9LWpkVjuLQ9HLAtcG7WwL8kpazDbRru4GZZ7e9dkWo4/H9LRue8rWTUeGwXNA7UutNNlTzdn166dODmhy/drZ+XiZYjXSJK83/jcUnegv/w8YBN2GHa/1ClQ1pNLSLyhp6VvTOgxnb0uQ+F6GY1PZfeNXfLefHyW1I/LkY43N2stC5gN7GgP74fvyuPYG+JWoz41FatDXWx9Yp4OfLe8bOnZs6e0b4nbW0U7aSvxPaN4mkiFnRb8+gObQ0alaquwLYVJ0e+evrH17/7+CP9PPQRlFDocyHjunk5WI86sC6GmBtRkxlrANm9ePuD+uHvP/tZ3/IFpSZ8rHjaJQ7e7sOE39EPjo1fn+NYIxjKXGcDVOHZVNKLc1Ojhv61PwHxRpPXvgsAAA=="
}
}

