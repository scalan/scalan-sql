package scalan.sql

import java.lang.reflect.Method
import scalan._
import scala.reflect.runtime.universe._
import scalan.sql.parser.SqlAST.{ComparisonOp, Index, SortDirection, Table}
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait ItersAbs extends Scalan with Iters {
  self: ItersDsl with ScalanSql =>

  // single proxy for each type family
  implicit def proxyIter[Row](p: Rep[Iter[Row]]): Iter[Row] = {
    proxyOps[Iter[Row]](p)(scala.reflect.classTag[Iter[Row]])
  }

  // familyElem
  class IterElem[Row, To <: Iter[Row]](implicit _eRow: Elem[Row])
    extends EntityElem[To] {
    def eRow = _eRow
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("Row" -> eRow)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[Iter[Row]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Iter[Row]] => convertIter(x) }
      tryConvert(element[Iter[Row]], this, x, conv)
    }

    def convertIter(x: Rep[Iter[Row]]): Rep[To] = {
      x.selfType1 match {
        case _: IterElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have IterElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iterElement[Row](implicit eRow: Elem[Row]): Elem[Iter[Row]] =
    cachedElem[IterElem[Row, Iter[Row]]](eRow)

  implicit case object IterCompanionElem extends CompanionElem[IterCompanionAbs] {
    lazy val tag = weakTypeTag[IterCompanionAbs]
    protected def getDefaultRep = Iter
  }

  abstract class IterCompanionAbs extends CompanionDef[IterCompanionAbs] with IterCompanion {
    def selfType = IterCompanionElem
    override def toString = "Iter"
  }
  def Iter: Rep[IterCompanionAbs]
  implicit def proxyIterCompanionAbs(p: Rep[IterCompanionAbs]): IterCompanionAbs =
    proxyOps[IterCompanionAbs](p)

  // single proxy for each type family
  implicit def proxyCursorIter[Row](p: Rep[CursorIter[Row]]): CursorIter[Row] = {
    proxyOps[CursorIter[Row]](p)(scala.reflect.classTag[CursorIter[Row]])
  }
  // familyElem
  class CursorIterElem[Row, To <: CursorIter[Row]](implicit _eRow: Elem[Row])
    extends IterElem[Row, To] {
    override def eRow = _eRow
    override lazy val parent: Option[Elem[_]] = Some(iterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[CursorIter[Row]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[CursorIter[Row]] => convertCursorIter(x) }
      tryConvert(element[CursorIter[Row]], this, x, conv)
    }

    def convertCursorIter(x: Rep[CursorIter[Row]]): Rep[To] = {
      x.selfType1 match {
        case _: CursorIterElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have CursorIterElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def cursorIterElement[Row](implicit eRow: Elem[Row]): Elem[CursorIter[Row]] =
    cachedElem[CursorIterElem[Row, CursorIter[Row]]](eRow)

  implicit case object CursorIterCompanionElem extends CompanionElem[CursorIterCompanionAbs] {
    lazy val tag = weakTypeTag[CursorIterCompanionAbs]
    protected def getDefaultRep = CursorIter
  }

  abstract class CursorIterCompanionAbs extends CompanionDef[CursorIterCompanionAbs] {
    def selfType = CursorIterCompanionElem
    override def toString = "CursorIter"
  }
  def CursorIter: Rep[CursorIterCompanionAbs]
  implicit def proxyCursorIterCompanionAbs(p: Rep[CursorIterCompanionAbs]): CursorIterCompanionAbs =
    proxyOps[CursorIterCompanionAbs](p)

  abstract class AbsTableIter[Row]
      (table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends TableIter[Row](table, scanId, direction, fakeDep, kernelInput) with Def[TableIter[Row]] {
    lazy val selfType = element[TableIter[Row]]
  }
  // elem for concrete class
  class TableIterElem[Row](val iso: Iso[TableIterData[Row], TableIter[Row]])(implicit override val eRow: Elem[Row])
    extends CursorIterElem[Row, TableIter[Row]]
    with ConcreteElem[TableIterData[Row], TableIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(cursorIterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertCursorIter(x: Rep[CursorIter[Row]]) = TableIter(x.table, x.scanId, x.direction, x.fakeDep, x.kernelInput)
    override def getDefaultRep = TableIter(element[Table].defaultRepValue, 0, element[SortDirection].defaultRepValue, (), element[KernelInput].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableIter[Row]]
    }
  }

  // state representation type
  type TableIterData[Row] = (Table, (Int, (SortDirection, (Unit, KernelInput))))

  // 3) Iso for concrete class
  class TableIterIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[TableIterData[Row], TableIter[Row]] with Def[TableIterIso[Row]] {
    override def from(p: Rep[TableIter[Row]]) =
      (p.table, p.scanId, p.direction, p.fakeDep, p.kernelInput)
    override def to(p: Rep[(Table, (Int, (SortDirection, (Unit, KernelInput))))]) = {
      val Pair(table, Pair(scanId, Pair(direction, Pair(fakeDep, kernelInput)))) = p
      TableIter(table, scanId, direction, fakeDep, kernelInput)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Int], pairElement(element[SortDirection], pairElement(element[Unit], element[KernelInput]))))
    lazy val eTo = new TableIterElem[Row](self)
    lazy val selfType = new TableIterIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class TableIterIsoElem[Row](eRow: Elem[Row]) extends Elem[TableIterIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new TableIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class TableIterCompanionAbs extends CompanionDef[TableIterCompanionAbs] {
    def selfType = TableIterCompanionElem
    override def toString = "TableIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[TableIterData[Row]])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
      isoTableIter(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
      mkTableIter(table, scanId, direction, fakeDep, kernelInput)

    def unapply[Row](p: Rep[CursorIter[Row]]) = unmkTableIter(p)
  }
  lazy val TableIterRep: Rep[TableIterCompanionAbs] = new TableIterCompanionAbs
  lazy val TableIter: TableIterCompanionAbs = proxyTableIterCompanion(TableIterRep)
  implicit def proxyTableIterCompanion(p: Rep[TableIterCompanionAbs]): TableIterCompanionAbs = {
    proxyOps[TableIterCompanionAbs](p)
  }

  implicit case object TableIterCompanionElem extends CompanionElem[TableIterCompanionAbs] {
    lazy val tag = weakTypeTag[TableIterCompanionAbs]
    protected def getDefaultRep = TableIter
  }

  implicit def proxyTableIter[Row](p: Rep[TableIter[Row]]): TableIter[Row] =
    proxyOps[TableIter[Row]](p)

  implicit class ExtendedTableIter[Row](p: Rep[TableIter[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[TableIterData[Row]] = isoTableIter(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoTableIter[Row](implicit eRow: Elem[Row]): Iso[TableIterData[Row], TableIter[Row]] =
    reifyObject(new TableIterIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkTableIter[Row](table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]]
  def unmkTableIter[Row](p: Rep[CursorIter[Row]]): Option[(Rep[Table], Rep[Int], Rep[SortDirection], Rep[Unit], Rep[KernelInput])]

  abstract class AbsIndexIter[Row]
      (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends IndexIter[Row](table, index, scanId, direction, fakeDep, kernelInput) with Def[IndexIter[Row]] {
    lazy val selfType = element[IndexIter[Row]]
  }
  // elem for concrete class
  class IndexIterElem[Row](val iso: Iso[IndexIterData[Row], IndexIter[Row]])(implicit override val eRow: Elem[Row])
    extends CursorIterElem[Row, IndexIter[Row]]
    with ConcreteElem[IndexIterData[Row], IndexIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(cursorIterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertCursorIter(x: Rep[CursorIter[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from CursorIter to IndexIter: missing fields List(index)")
    override def getDefaultRep = IndexIter(element[Table].defaultRepValue, element[Index].defaultRepValue, 0, element[SortDirection].defaultRepValue, (), element[KernelInput].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexIter[Row]]
    }
  }

  // state representation type
  type IndexIterData[Row] = (Table, (Index, (Int, (SortDirection, (Unit, KernelInput)))))

  // 3) Iso for concrete class
  class IndexIterIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[IndexIterData[Row], IndexIter[Row]] with Def[IndexIterIso[Row]] {
    override def from(p: Rep[IndexIter[Row]]) =
      (p.table, p.index, p.scanId, p.direction, p.fakeDep, p.kernelInput)
    override def to(p: Rep[(Table, (Index, (Int, (SortDirection, (Unit, KernelInput)))))]) = {
      val Pair(table, Pair(index, Pair(scanId, Pair(direction, Pair(fakeDep, kernelInput))))) = p
      IndexIter(table, index, scanId, direction, fakeDep, kernelInput)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Index], pairElement(element[Int], pairElement(element[SortDirection], pairElement(element[Unit], element[KernelInput])))))
    lazy val eTo = new IndexIterElem[Row](self)
    lazy val selfType = new IndexIterIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class IndexIterIsoElem[Row](eRow: Elem[Row]) extends Elem[IndexIterIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IndexIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class IndexIterCompanionAbs extends CompanionDef[IndexIterCompanionAbs] {
    def selfType = IndexIterCompanionElem
    override def toString = "IndexIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[IndexIterData[Row]])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
      isoIndexIter(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
      mkIndexIter(table, index, scanId, direction, fakeDep, kernelInput)

    def unapply[Row](p: Rep[CursorIter[Row]]) = unmkIndexIter(p)
  }
  lazy val IndexIterRep: Rep[IndexIterCompanionAbs] = new IndexIterCompanionAbs
  lazy val IndexIter: IndexIterCompanionAbs = proxyIndexIterCompanion(IndexIterRep)
  implicit def proxyIndexIterCompanion(p: Rep[IndexIterCompanionAbs]): IndexIterCompanionAbs = {
    proxyOps[IndexIterCompanionAbs](p)
  }

  implicit case object IndexIterCompanionElem extends CompanionElem[IndexIterCompanionAbs] {
    lazy val tag = weakTypeTag[IndexIterCompanionAbs]
    protected def getDefaultRep = IndexIter
  }

  implicit def proxyIndexIter[Row](p: Rep[IndexIter[Row]]): IndexIter[Row] =
    proxyOps[IndexIter[Row]](p)

  implicit class ExtendedIndexIter[Row](p: Rep[IndexIter[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[IndexIterData[Row]] = isoIndexIter(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexIter[Row](implicit eRow: Elem[Row]): Iso[IndexIterData[Row], IndexIter[Row]] =
    reifyObject(new IndexIterIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkIndexIter[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]]
  def unmkIndexIter[Row](p: Rep[CursorIter[Row]]): Option[(Rep[Table], Rep[Index], Rep[Int], Rep[SortDirection], Rep[Unit], Rep[KernelInput])]

  abstract class AbsTableIterByRowids[Row, A]
      (table: Rep[Table], scanId: Rep[Int], sourceIter: RIter[A], f: Rep[A => Long])(implicit eRow: Elem[Row], eA: Elem[A])
    extends TableIterByRowids[Row, A](table, scanId, sourceIter, f) with Def[TableIterByRowids[Row, A]] {
    lazy val selfType = element[TableIterByRowids[Row, A]]
  }
  // elem for concrete class
  class TableIterByRowidsElem[Row, A](val iso: Iso[TableIterByRowidsData[Row, A], TableIterByRowids[Row, A]])(implicit override val eRow: Elem[Row], val eA: Elem[A])
    extends IterElem[Row, TableIterByRowids[Row, A]]
    with ConcreteElem[TableIterByRowidsData[Row, A], TableIterByRowids[Row, A]] {
    override lazy val parent: Option[Elem[_]] = Some(iterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow, "A" -> eA)

    override def convertIter(x: Rep[Iter[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Iter to TableIterByRowids: missing fields List(table, scanId, sourceIter, f)")
    override def getDefaultRep = TableIterByRowids(element[Table].defaultRepValue, 0, element[Iter[A]].defaultRepValue, constFun[A, Long](0l))
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      implicit val tagA = eA.tag
      weakTypeTag[TableIterByRowids[Row, A]]
    }
  }

  // state representation type
  type TableIterByRowidsData[Row, A] = (Table, (Int, (Iter[A], A => Long)))

  // 3) Iso for concrete class
  class TableIterByRowidsIso[Row, A](implicit eRow: Elem[Row], eA: Elem[A])
    extends EntityIso[TableIterByRowidsData[Row, A], TableIterByRowids[Row, A]] with Def[TableIterByRowidsIso[Row, A]] {
    override def from(p: Rep[TableIterByRowids[Row, A]]) =
      (p.table, p.scanId, p.sourceIter, p.f)
    override def to(p: Rep[(Table, (Int, (Iter[A], A => Long)))]) = {
      val Pair(table, Pair(scanId, Pair(sourceIter, f))) = p
      TableIterByRowids(table, scanId, sourceIter, f)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Int], pairElement(element[Iter[A]], element[A => Long])))
    lazy val eTo = new TableIterByRowidsElem[Row, A](self)
    lazy val selfType = new TableIterByRowidsIsoElem[Row, A](eRow, eA)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eRow
      case 1 => eA
    }
  }
  case class TableIterByRowidsIsoElem[Row, A](eRow: Elem[Row], eA: Elem[A]) extends Elem[TableIterByRowidsIso[Row, A]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new TableIterByRowidsIso[Row, A]()(eRow, eA))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      implicit val tagA = eA.tag
      weakTypeTag[TableIterByRowidsIso[Row, A]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow, "A" -> eA)
  }
  // 4) constructor and deconstructor
  class TableIterByRowidsCompanionAbs extends CompanionDef[TableIterByRowidsCompanionAbs] {
    def selfType = TableIterByRowidsCompanionElem
    override def toString = "TableIterByRowids"
    @scalan.OverloadId("fromData")
    def apply[Row, A](p: Rep[TableIterByRowidsData[Row, A]])(implicit eRow: Elem[Row], eA: Elem[A]): Rep[TableIterByRowids[Row, A]] =
      isoTableIterByRowids(eRow, eA).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row, A](table: Rep[Table], scanId: Rep[Int], sourceIter: RIter[A], f: Rep[A => Long])(implicit eRow: Elem[Row], eA: Elem[A]): Rep[TableIterByRowids[Row, A]] =
      mkTableIterByRowids(table, scanId, sourceIter, f)

    def unapply[Row, A](p: Rep[Iter[Row]]) = unmkTableIterByRowids(p)
  }
  lazy val TableIterByRowidsRep: Rep[TableIterByRowidsCompanionAbs] = new TableIterByRowidsCompanionAbs
  lazy val TableIterByRowids: TableIterByRowidsCompanionAbs = proxyTableIterByRowidsCompanion(TableIterByRowidsRep)
  implicit def proxyTableIterByRowidsCompanion(p: Rep[TableIterByRowidsCompanionAbs]): TableIterByRowidsCompanionAbs = {
    proxyOps[TableIterByRowidsCompanionAbs](p)
  }

  implicit case object TableIterByRowidsCompanionElem extends CompanionElem[TableIterByRowidsCompanionAbs] {
    lazy val tag = weakTypeTag[TableIterByRowidsCompanionAbs]
    protected def getDefaultRep = TableIterByRowids
  }

  implicit def proxyTableIterByRowids[Row, A](p: Rep[TableIterByRowids[Row, A]]): TableIterByRowids[Row, A] =
    proxyOps[TableIterByRowids[Row, A]](p)

  implicit class ExtendedTableIterByRowids[Row, A](p: Rep[TableIterByRowids[Row, A]])(implicit eRow: Elem[Row], eA: Elem[A]) {
    def toData: Rep[TableIterByRowidsData[Row, A]] = isoTableIterByRowids(eRow, eA).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoTableIterByRowids[Row, A](implicit eRow: Elem[Row], eA: Elem[A]): Iso[TableIterByRowidsData[Row, A], TableIterByRowids[Row, A]] =
    reifyObject(new TableIterByRowidsIso[Row, A]()(eRow, eA))

  // 6) smart constructor and deconstructor
  def mkTableIterByRowids[Row, A](table: Rep[Table], scanId: Rep[Int], sourceIter: RIter[A], f: Rep[A => Long])(implicit eRow: Elem[Row], eA: Elem[A]): Rep[TableIterByRowids[Row, A]]
  def unmkTableIterByRowids[Row, A](p: Rep[Iter[Row]]): Option[(Rep[Table], Rep[Int], Rep[Iter[A]], Rep[A => Long])]

  registerModule(Iters_Module)
}

// Std -----------------------------------
trait ItersStd extends ScalanStd with ItersDsl {
  self: ItersDsl with ScalanSqlStd =>

  lazy val Iter: Rep[IterCompanionAbs] = new IterCompanionAbs {
  }

  lazy val CursorIter: Rep[CursorIterCompanionAbs] = new CursorIterCompanionAbs {
  }

  case class StdTableIter[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val fakeDep: Rep[Unit], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsTableIter[Row](table, scanId, direction, fakeDep, kernelInput) {
  }

  def mkTableIter[Row]
    (table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
    new StdTableIter[Row](table, scanId, direction, fakeDep, kernelInput)
  def unmkTableIter[Row](p: Rep[CursorIter[Row]]) = p match {
    case p: TableIter[Row] @unchecked =>
      Some((p.table, p.scanId, p.direction, p.fakeDep, p.kernelInput))
    case _ => None
  }

  case class StdIndexIter[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val fakeDep: Rep[Unit], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsIndexIter[Row](table, index, scanId, direction, fakeDep, kernelInput) {
  }

  def mkIndexIter[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
    new StdIndexIter[Row](table, index, scanId, direction, fakeDep, kernelInput)
  def unmkIndexIter[Row](p: Rep[CursorIter[Row]]) = p match {
    case p: IndexIter[Row] @unchecked =>
      Some((p.table, p.index, p.scanId, p.direction, p.fakeDep, p.kernelInput))
    case _ => None
  }

  case class StdTableIterByRowids[Row, A]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val sourceIter: RIter[A], override val f: Rep[A => Long])(implicit eRow: Elem[Row], eA: Elem[A])
    extends AbsTableIterByRowids[Row, A](table, scanId, sourceIter, f) {
  }

  def mkTableIterByRowids[Row, A]
    (table: Rep[Table], scanId: Rep[Int], sourceIter: RIter[A], f: Rep[A => Long])(implicit eRow: Elem[Row], eA: Elem[A]): Rep[TableIterByRowids[Row, A]] =
    new StdTableIterByRowids[Row, A](table, scanId, sourceIter, f)
  def unmkTableIterByRowids[Row, A](p: Rep[Iter[Row]]) = p match {
    case p: TableIterByRowids[Row, A] @unchecked =>
      Some((p.table, p.scanId, p.sourceIter, p.f))
    case _ => None
  }
}

// Exp -----------------------------------
trait ItersExp extends ScalanExp with ItersDsl {
  self: ItersDsl with ScalanSqlExp =>

  lazy val Iter: Rep[IterCompanionAbs] = new IterCompanionAbs {
  }

  lazy val CursorIter: Rep[CursorIterCompanionAbs] = new CursorIterCompanionAbs {
  }

  object CursorIterMethods {
    object table {
      def unapply(d: Def[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "table" =>
          Some(receiver).asInstanceOf[Option[Rep[CursorIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object scanId {
      def unapply(d: Def[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "scanId" =>
          Some(receiver).asInstanceOf[Option[Rep[CursorIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object direction {
      def unapply(d: Def[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "direction" =>
          Some(receiver).asInstanceOf[Option[Rep[CursorIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fakeDep {
      def unapply(d: Def[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "fakeDep" =>
          Some(receiver).asInstanceOf[Option[Rep[CursorIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object kernelInput {
      def unapply(d: Def[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "kernelInput" =>
          Some(receiver).asInstanceOf[Option[Rep[CursorIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[CursorIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object seekIndex {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(keyValues, operation, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "seekIndex" =>
          Some((receiver, keyValues, operation)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  case class ExpTableIter[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val fakeDep: Rep[Unit], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsTableIter[Row](table, scanId, direction, fakeDep, kernelInput)

  object TableIterMethods {
  }

  def mkTableIter[Row]
    (table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
    new ExpTableIter[Row](table, scanId, direction, fakeDep, kernelInput)
  def unmkTableIter[Row](p: Rep[CursorIter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TableIterElem[Row] @unchecked =>
      Some((p.asRep[TableIter[Row]].table, p.asRep[TableIter[Row]].scanId, p.asRep[TableIter[Row]].direction, p.asRep[TableIter[Row]].fakeDep, p.asRep[TableIter[Row]].kernelInput))
    case _ =>
      None
  }

  case class ExpIndexIter[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val fakeDep: Rep[Unit], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsIndexIter[Row](table, index, scanId, direction, fakeDep, kernelInput)

  object IndexIterMethods {
    // WARNING: Cannot generate matcher for method `isCovering`: Method's return type Boolean is not a Rep
  }

  def mkIndexIter[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], fakeDep: Rep[Unit], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
    new ExpIndexIter[Row](table, index, scanId, direction, fakeDep, kernelInput)
  def unmkIndexIter[Row](p: Rep[CursorIter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexIterElem[Row] @unchecked =>
      Some((p.asRep[IndexIter[Row]].table, p.asRep[IndexIter[Row]].index, p.asRep[IndexIter[Row]].scanId, p.asRep[IndexIter[Row]].direction, p.asRep[IndexIter[Row]].fakeDep, p.asRep[IndexIter[Row]].kernelInput))
    case _ =>
      None
  }

  case class ExpTableIterByRowids[Row, A]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val sourceIter: RIter[A], override val f: Rep[A => Long])(implicit eRow: Elem[Row], eA: Elem[A])
    extends AbsTableIterByRowids[Row, A](table, scanId, sourceIter, f)

  object TableIterByRowidsMethods {
  }

  def mkTableIterByRowids[Row, A]
    (table: Rep[Table], scanId: Rep[Int], sourceIter: RIter[A], f: Rep[A => Long])(implicit eRow: Elem[Row], eA: Elem[A]): Rep[TableIterByRowids[Row, A]] =
    new ExpTableIterByRowids[Row, A](table, scanId, sourceIter, f)
  def unmkTableIterByRowids[Row, A](p: Rep[Iter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TableIterByRowidsElem[Row, A] @unchecked =>
      Some((p.asRep[TableIterByRowids[Row, A]].table, p.asRep[TableIterByRowids[Row, A]].scanId, p.asRep[TableIterByRowids[Row, A]].sourceIter, p.asRep[TableIterByRowids[Row, A]].f))
    case _ =>
      None
  }

  object IterMethods {
    object map {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapU {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Elem[B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, eB, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "mapU" =>
          Some((receiver, f, eB)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Elem[B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Elem[B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "flatMap" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "filter" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object takeWhile {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "takeWhile" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[Iter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[Iter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Iter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceU {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "reduceU" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "mapReduce" =>
          Some((receiver, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduceU {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "mapReduceU" =>
          Some((receiver, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object partialMapReduce {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "partialMapReduce" =>
          Some((receiver, prefixComparator, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object partialMapReduceU {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "partialMapReduceU" =>
          Some((receiver, prefixComparator, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sort {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "sort" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "sortBy" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object partialSort {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, suffixComparator, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "partialSort" =>
          Some((receiver, prefixComparator, suffixComparator)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object join {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, cloneOther, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "join" =>
          Some((receiver, other, thisKey, otherKey, cloneOther)).asInstanceOf[Option[(Rep[Iter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toArray`: Method's return type Arr[Row] is not a Rep

    object materialize {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Row]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "materialize" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Row]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Row]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IterCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == IterCompanionElem && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Iters_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAN1XTWwbRRSedew4jtM2TVU1kQgNwYVSgR1RoYIihNLEARfnR9mEorRKNd4dp9vszk52x8HmUG6VgBtCIJA4FIG4VCDEDSQugIRQ4IA4IHHiwKn8VEWiEhKIN7M//onXCRagCh9G45m37+d733szc+0nlHAddI+rYRPTrEU4zqpyPuXyjJqn3OC1OVuvmGSGlNeeePP3c9bzR2JocBX1XsTujGuuopQ3yVdZOFe5XkQpTDXicttxObqrKC3kNNs0icYNm+YMy6pwXDJJrmi4fLKI4iVbr22iy0gpokHNpppDOFGnTey6xPXX+4jwyAj/p+T/2gKr26A5EUWuIYplBxsc3Acbg578EmFqjdq0ZnG033dtgQm3QCZNqgxiKFjMlGZ6iihpWMx2eGA1CRYu2nrwN04xLKCh4iW8hXNgdT2ncseg60IZw9oGXifzICLE4xCDS8zyco0RX3na5XqTvSpDCEFWHpSOZeuYZUPMsgKzjEocA5vGs1hsLjp2tYa8n9KDUJWBivt3URFoIHmqZ144r527paatmPi4KlxJSod6QdHRCIbI9AC2ny+95N58/OqpGOpfRf2GO1VyuYM13kgDH640ptTm0ucQQeysQwbHozIorUyBTAtNUpptMUxBk4/lACTKNDSDC2GxNuCnJwL7JGckEFWqTAnjHYuIV3JpGpvm4vWRB479mH86hmLNJlKgUoVicAKlHPVPVxzXdgqcOL4BMR7gqGfJfkbiLIZUtT4mO7gQgnHv9Z/1zybQ+VgIoW9xb1kDFUMPv/7hMbL4bgz1rUqSz5p4XeZPYDRDXG0V9dlbxPHWk1vYFLO2OUzqpIwrJveRbYSkByDhaCyyPhkReE1K3itB+GmPuvM2JZnZxcxv6hcvXxPMdNCAt+MV7J/GqT++21/mkrQcJSS36/gS1ox4YlnsN2AuN4ZDw2IY5SCvYVrQ2+hx0N1R1GBk0TEsaE9b5KFPPlq58fF8QrJjyEfmKWxWiNcZfGDqIAnflQmwVKC8k2Mp3XC8So6OcZ8KvWomkNs11mQZb0CuWYTC+ziKHYchvkKNjq6lN4hDiVmgrMKjnUs/WZdqT32pToofbvh0WGkxFydQPIHWeN4k1h5qCwCUBBClWLcjsjoanVUoFHpEnXv1vaNrMZQ4gxJlqAG3iBIlu0L1oPPAicVJlZ8O1pTmGoBOgx1sBSXq9ekxJJ2Qbu7wWIqmleaoumklO6Bszdw/VDQJg+qk2kFNQex3X3tiOHHbFIcYHvkf1INMSms9iHG+W47Gb0t2dk+rfteuOBoJoxI2l3bGqEzt6oNSbnui3Bnde2YrVPum8NqhA6MXvpeHSa9uW9iQxB2BFuTAbVPGPlJlLY6E3XvChJwUbe/U+Q+o1V5JjEx1UNEEXsDNg2GvPl0DE4aXvL1zNFqkjbldL03BM+L9K1cO33jrwiF51e0rGdzCLDPxNy66Yj7w715kUUuCwO3ua7SpUkMJcUfr925iqm2Rg+M3jbWrL3J5GVWqzc+hhdIl6LeTUs8dUs82alDUolgmCu5D+4SP043AeOljvvtdN6xt1D0Ykd42nMzdOFYfv6x//i1gnI2g4wzRTOwQXbwbiQXvWo9oJ1957OyZ4bMrMg8DuhTydsIbePtX+Bxmk/LNeLzDmxGEMnmL8ZqYnPz00a+f237nbXn1rgMiDnqBJ/fpQbPuphmGMx4RjurzGZh6+dYb8ye++uAH2e/6RWXAG4CGr+/GS3Nz0vqkXXhL1xMFqynPirppNsANSRcV5Lslxl/E8OtfG1H8RBERAAA="
}
}

