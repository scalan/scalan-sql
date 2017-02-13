package scalan.sql

import java.lang.reflect.Method
import scalan._
import scala.reflect.runtime.universe._
import scalan.sql.parser.SqlAST._
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
    lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
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
    override lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
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

  // single proxy for each type family
  implicit def proxyAtMostOne[Row](p: Rep[AtMostOne[Row]]): AtMostOne[Row] = {
    proxyOps[AtMostOne[Row]](p)(scala.reflect.classTag[AtMostOne[Row]])
  }
  // familyElem
  class AtMostOneElem[Row, To <: AtMostOne[Row]](implicit _eRow: Elem[Row])
    extends IterElem[Row, To] {
    override def eRow = _eRow
    override lazy val parent: Option[Elem[_]] = Some(iterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[AtMostOne[Row]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[AtMostOne[Row]] => convertAtMostOne(x) }
      tryConvert(element[AtMostOne[Row]], this, x, conv)
    }

    def convertAtMostOne(x: Rep[AtMostOne[Row]]): Rep[To] = {
      x.selfType1 match {
        case _: AtMostOneElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have AtMostOneElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def atMostOneElement[Row](implicit eRow: Elem[Row]): Elem[AtMostOne[Row]] =
    cachedElem[AtMostOneElem[Row, AtMostOne[Row]]](eRow)

  implicit case object AtMostOneCompanionElem extends CompanionElem[AtMostOneCompanionAbs] {
    lazy val tag = weakTypeTag[AtMostOneCompanionAbs]
    protected def getDefaultRep = AtMostOne
  }

  abstract class AtMostOneCompanionAbs extends CompanionDef[AtMostOneCompanionAbs] {
    def selfType = AtMostOneCompanionElem
    override def toString = "AtMostOne"
  }
  def AtMostOne: Rep[AtMostOneCompanionAbs]
  implicit def proxyAtMostOneCompanionAbs(p: Rep[AtMostOneCompanionAbs]): AtMostOneCompanionAbs =
    proxyOps[AtMostOneCompanionAbs](p)

  abstract class AbsTableIter[Row]
      (table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends TableIter[Row](table, scanId, direction, kernelInput) with Def[TableIter[Row]] {
    lazy val selfType = element[TableIter[Row]]
  }
  // elem for concrete class
  class TableIterElem[Row](val iso: Iso[TableIterData[Row], TableIter[Row]])(implicit override val eRow: Elem[Row])
    extends CursorIterElem[Row, TableIter[Row]]
    with ConcreteElem[TableIterData[Row], TableIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(cursorIterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))

    override def convertCursorIter(x: Rep[CursorIter[Row]]) = TableIter(x.table, x.scanId, x.direction, x.kernelInput)
    override def getDefaultRep = TableIter(element[Table].defaultRepValue, 0, element[SortDirection].defaultRepValue, element[KernelInput].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableIter[Row]]
    }
  }

  // state representation type
  type TableIterData[Row] = (Table, (Int, (SortDirection, KernelInput)))

  // 3) Iso for concrete class
  class TableIterIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[TableIterData[Row], TableIter[Row]] with Def[TableIterIso[Row]] {
    override def from(p: Rep[TableIter[Row]]) =
      (p.table, p.scanId, p.direction, p.kernelInput)
    override def to(p: Rep[(Table, (Int, (SortDirection, KernelInput)))]) = {
      val Pair(table, Pair(scanId, Pair(direction, kernelInput))) = p
      TableIter(table, scanId, direction, kernelInput)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Int], pairElement(element[SortDirection], element[KernelInput])))
    lazy val eTo = new TableIterElem[Row](self)
    lazy val selfType = new TableIterIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class TableIterIsoElem[Row](eRow: Elem[Row]) extends Elem[TableIterIso[Row]] {
    def getDefaultRep = reifyObject(new TableIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class TableIterCompanionAbs extends CompanionDef[TableIterCompanionAbs] {
    def selfType = TableIterCompanionElem
    override def toString = "TableIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[TableIterData[Row]])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
      isoTableIter(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
      mkTableIter(table, scanId, direction, kernelInput)

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
  def mkTableIter[Row](table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]]
  def unmkTableIter[Row](p: Rep[CursorIter[Row]]): Option[(Rep[Table], Rep[Int], Rep[SortDirection], Rep[KernelInput])]

  abstract class AbsIndexIter[Row]
      (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends IndexIter[Row](table, index, scanId, direction, kernelInput) with Def[IndexIter[Row]] {
    lazy val selfType = element[IndexIter[Row]]
  }
  // elem for concrete class
  class IndexIterElem[Row](val iso: Iso[IndexIterData[Row], IndexIter[Row]])(implicit override val eRow: Elem[Row])
    extends CursorIterElem[Row, IndexIter[Row]]
    with ConcreteElem[IndexIterData[Row], IndexIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(cursorIterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))

    override def convertCursorIter(x: Rep[CursorIter[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from CursorIter to IndexIter: missing fields List(index)")
    override def getDefaultRep = IndexIter(element[Table].defaultRepValue, element[Index].defaultRepValue, 0, element[SortDirection].defaultRepValue, element[KernelInput].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexIter[Row]]
    }
  }

  // state representation type
  type IndexIterData[Row] = (Table, (Index, (Int, (SortDirection, KernelInput))))

  // 3) Iso for concrete class
  class IndexIterIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[IndexIterData[Row], IndexIter[Row]] with Def[IndexIterIso[Row]] {
    override def from(p: Rep[IndexIter[Row]]) =
      (p.table, p.index, p.scanId, p.direction, p.kernelInput)
    override def to(p: Rep[(Table, (Index, (Int, (SortDirection, KernelInput))))]) = {
      val Pair(table, Pair(index, Pair(scanId, Pair(direction, kernelInput)))) = p
      IndexIter(table, index, scanId, direction, kernelInput)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Index], pairElement(element[Int], pairElement(element[SortDirection], element[KernelInput]))))
    lazy val eTo = new IndexIterElem[Row](self)
    lazy val selfType = new IndexIterIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class IndexIterIsoElem[Row](eRow: Elem[Row]) extends Elem[IndexIterIso[Row]] {
    def getDefaultRep = reifyObject(new IndexIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class IndexIterCompanionAbs extends CompanionDef[IndexIterCompanionAbs] {
    def selfType = IndexIterCompanionElem
    override def toString = "IndexIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[IndexIterData[Row]])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
      isoIndexIter(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
      mkIndexIter(table, index, scanId, direction, kernelInput)

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
  def mkIndexIter[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]]
  def unmkIndexIter[Row](p: Rep[CursorIter[Row]]): Option[(Rep[Table], Rep[Index], Rep[Int], Rep[SortDirection], Rep[KernelInput])]

  abstract class AbsSingletonIter[Row]
      (value: Rep[Row])(implicit eRow: Elem[Row])
    extends SingletonIter[Row](value) with Def[SingletonIter[Row]] {
    lazy val selfType = element[SingletonIter[Row]]
  }
  // elem for concrete class
  class SingletonIterElem[Row](val iso: Iso[SingletonIterData[Row], SingletonIter[Row]])(implicit override val eRow: Elem[Row])
    extends AtMostOneElem[Row, SingletonIter[Row]]
    with ConcreteElem[SingletonIterData[Row], SingletonIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(atMostOneElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))

    override def convertAtMostOne(x: Rep[AtMostOne[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from AtMostOne to SingletonIter: missing fields List(value)")
    override def getDefaultRep = SingletonIter(element[Row].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[SingletonIter[Row]]
    }
  }

  // state representation type
  type SingletonIterData[Row] = Row

  // 3) Iso for concrete class
  class SingletonIterIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[SingletonIterData[Row], SingletonIter[Row]] with Def[SingletonIterIso[Row]] {
    override def from(p: Rep[SingletonIter[Row]]) =
      p.value
    override def to(p: Rep[Row]) = {
      val value = p
      SingletonIter(value)
    }
    lazy val eFrom = element[Row]
    lazy val eTo = new SingletonIterElem[Row](self)
    lazy val selfType = new SingletonIterIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class SingletonIterIsoElem[Row](eRow: Elem[Row]) extends Elem[SingletonIterIso[Row]] {
    def getDefaultRep = reifyObject(new SingletonIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[SingletonIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class SingletonIterCompanionAbs extends CompanionDef[SingletonIterCompanionAbs] {
    def selfType = SingletonIterCompanionElem
    override def toString = "SingletonIter"

    @scalan.OverloadId("fromFields")
    def apply[Row](value: Rep[Row])(implicit eRow: Elem[Row]): Rep[SingletonIter[Row]] =
      mkSingletonIter(value)

    def unapply[Row](p: Rep[AtMostOne[Row]]) = unmkSingletonIter(p)
  }
  lazy val SingletonIterRep: Rep[SingletonIterCompanionAbs] = new SingletonIterCompanionAbs
  lazy val SingletonIter: SingletonIterCompanionAbs = proxySingletonIterCompanion(SingletonIterRep)
  implicit def proxySingletonIterCompanion(p: Rep[SingletonIterCompanionAbs]): SingletonIterCompanionAbs = {
    proxyOps[SingletonIterCompanionAbs](p)
  }

  implicit case object SingletonIterCompanionElem extends CompanionElem[SingletonIterCompanionAbs] {
    lazy val tag = weakTypeTag[SingletonIterCompanionAbs]
    protected def getDefaultRep = SingletonIter
  }

  implicit def proxySingletonIter[Row](p: Rep[SingletonIter[Row]]): SingletonIter[Row] =
    proxyOps[SingletonIter[Row]](p)

  implicit class ExtendedSingletonIter[Row](p: Rep[SingletonIter[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[SingletonIterData[Row]] = isoSingletonIter(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoSingletonIter[Row](implicit eRow: Elem[Row]): Iso[SingletonIterData[Row], SingletonIter[Row]] =
    reifyObject(new SingletonIterIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkSingletonIter[Row](value: Rep[Row])(implicit eRow: Elem[Row]): Rep[SingletonIter[Row]]
  def unmkSingletonIter[Row](p: Rep[AtMostOne[Row]]): Option[(Rep[Row])]

  abstract class AbsEmptyIter[Row]
      ()(implicit eRow: Elem[Row])
    extends EmptyIter[Row]() with Def[EmptyIter[Row]] {
    lazy val selfType = element[EmptyIter[Row]]
  }
  // elem for concrete class
  class EmptyIterElem[Row](val iso: Iso[EmptyIterData[Row], EmptyIter[Row]])(implicit override val eRow: Elem[Row])
    extends AtMostOneElem[Row, EmptyIter[Row]]
    with ConcreteElem[EmptyIterData[Row], EmptyIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(atMostOneElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))

    override def convertAtMostOne(x: Rep[AtMostOne[Row]]) = EmptyIter()
    override def getDefaultRep = EmptyIter()
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[EmptyIter[Row]]
    }
  }

  // state representation type
  type EmptyIterData[Row] = Unit

  // 3) Iso for concrete class
  class EmptyIterIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[EmptyIterData[Row], EmptyIter[Row]] with Def[EmptyIterIso[Row]] {
    override def from(p: Rep[EmptyIter[Row]]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      EmptyIter()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new EmptyIterElem[Row](self)
    lazy val selfType = new EmptyIterIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class EmptyIterIsoElem[Row](eRow: Elem[Row]) extends Elem[EmptyIterIso[Row]] {
    def getDefaultRep = reifyObject(new EmptyIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[EmptyIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class EmptyIterCompanionAbs extends CompanionDef[EmptyIterCompanionAbs] {
    def selfType = EmptyIterCompanionElem
    override def toString = "EmptyIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[EmptyIterData[Row]])(implicit eRow: Elem[Row]): Rep[EmptyIter[Row]] =
      isoEmptyIter(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row]()(implicit eRow: Elem[Row]): Rep[EmptyIter[Row]] =
      mkEmptyIter()

    def unapply[Row](p: Rep[AtMostOne[Row]]) = unmkEmptyIter(p)
  }
  lazy val EmptyIterRep: Rep[EmptyIterCompanionAbs] = new EmptyIterCompanionAbs
  lazy val EmptyIter: EmptyIterCompanionAbs = proxyEmptyIterCompanion(EmptyIterRep)
  implicit def proxyEmptyIterCompanion(p: Rep[EmptyIterCompanionAbs]): EmptyIterCompanionAbs = {
    proxyOps[EmptyIterCompanionAbs](p)
  }

  implicit case object EmptyIterCompanionElem extends CompanionElem[EmptyIterCompanionAbs] {
    lazy val tag = weakTypeTag[EmptyIterCompanionAbs]
    protected def getDefaultRep = EmptyIter
  }

  implicit def proxyEmptyIter[Row](p: Rep[EmptyIter[Row]]): EmptyIter[Row] =
    proxyOps[EmptyIter[Row]](p)

  implicit class ExtendedEmptyIter[Row](p: Rep[EmptyIter[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[EmptyIterData[Row]] = isoEmptyIter(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoEmptyIter[Row](implicit eRow: Elem[Row]): Iso[EmptyIterData[Row], EmptyIter[Row]] =
    reifyObject(new EmptyIterIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkEmptyIter[Row]()(implicit eRow: Elem[Row]): Rep[EmptyIter[Row]]
  def unmkEmptyIter[Row](p: Rep[AtMostOne[Row]]): Option[(Rep[Unit])]

  abstract class AbsConditionalIter[Row]
      (condition: Rep[Boolean], baseIter: RIter[Row])(implicit eRow: Elem[Row])
    extends ConditionalIter[Row](condition, baseIter) with Def[ConditionalIter[Row]] {
    lazy val selfType = element[ConditionalIter[Row]]
  }
  // elem for concrete class
  class ConditionalIterElem[Row](val iso: Iso[ConditionalIterData[Row], ConditionalIter[Row]])(implicit override val eRow: Elem[Row])
    extends IterElem[Row, ConditionalIter[Row]]
    with ConcreteElem[ConditionalIterData[Row], ConditionalIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(iterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))

    override def convertIter(x: Rep[Iter[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Iter to ConditionalIter: missing fields List(condition, baseIter)")
    override def getDefaultRep = ConditionalIter(false, element[Iter[Row]].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[ConditionalIter[Row]]
    }
  }

  // state representation type
  type ConditionalIterData[Row] = (Boolean, Iter[Row])

  // 3) Iso for concrete class
  class ConditionalIterIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[ConditionalIterData[Row], ConditionalIter[Row]] with Def[ConditionalIterIso[Row]] {
    override def from(p: Rep[ConditionalIter[Row]]) =
      (p.condition, p.baseIter)
    override def to(p: Rep[(Boolean, Iter[Row])]) = {
      val Pair(condition, baseIter) = p
      ConditionalIter(condition, baseIter)
    }
    lazy val eFrom = pairElement(element[Boolean], element[Iter[Row]])
    lazy val eTo = new ConditionalIterElem[Row](self)
    lazy val selfType = new ConditionalIterIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class ConditionalIterIsoElem[Row](eRow: Elem[Row]) extends Elem[ConditionalIterIso[Row]] {
    def getDefaultRep = reifyObject(new ConditionalIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[ConditionalIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> (eRow -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class ConditionalIterCompanionAbs extends CompanionDef[ConditionalIterCompanionAbs] {
    def selfType = ConditionalIterCompanionElem
    override def toString = "ConditionalIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[ConditionalIterData[Row]])(implicit eRow: Elem[Row]): Rep[ConditionalIter[Row]] =
      isoConditionalIter(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](condition: Rep[Boolean], baseIter: RIter[Row])(implicit eRow: Elem[Row]): Rep[ConditionalIter[Row]] =
      mkConditionalIter(condition, baseIter)

    def unapply[Row](p: Rep[Iter[Row]]) = unmkConditionalIter(p)
  }
  lazy val ConditionalIterRep: Rep[ConditionalIterCompanionAbs] = new ConditionalIterCompanionAbs
  lazy val ConditionalIter: ConditionalIterCompanionAbs = proxyConditionalIterCompanion(ConditionalIterRep)
  implicit def proxyConditionalIterCompanion(p: Rep[ConditionalIterCompanionAbs]): ConditionalIterCompanionAbs = {
    proxyOps[ConditionalIterCompanionAbs](p)
  }

  implicit case object ConditionalIterCompanionElem extends CompanionElem[ConditionalIterCompanionAbs] {
    lazy val tag = weakTypeTag[ConditionalIterCompanionAbs]
    protected def getDefaultRep = ConditionalIter
  }

  implicit def proxyConditionalIter[Row](p: Rep[ConditionalIter[Row]]): ConditionalIter[Row] =
    proxyOps[ConditionalIter[Row]](p)

  implicit class ExtendedConditionalIter[Row](p: Rep[ConditionalIter[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[ConditionalIterData[Row]] = isoConditionalIter(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoConditionalIter[Row](implicit eRow: Elem[Row]): Iso[ConditionalIterData[Row], ConditionalIter[Row]] =
    reifyObject(new ConditionalIterIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkConditionalIter[Row](condition: Rep[Boolean], baseIter: RIter[Row])(implicit eRow: Elem[Row]): Rep[ConditionalIter[Row]]
  def unmkConditionalIter[Row](p: Rep[Iter[Row]]): Option[(Rep[Boolean], Rep[Iter[Row]])]

  registerModule(Iters_Module)
}

// Std -----------------------------------
trait ItersStd extends ScalanStd with ItersDsl {
  self: ItersDsl with ScalanSqlStd =>

  lazy val Iter: Rep[IterCompanionAbs] = new IterCompanionAbs {
  }

  lazy val CursorIter: Rep[CursorIterCompanionAbs] = new CursorIterCompanionAbs {
  }

  lazy val AtMostOne: Rep[AtMostOneCompanionAbs] = new AtMostOneCompanionAbs {
  }

  case class StdTableIter[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsTableIter[Row](table, scanId, direction, kernelInput) {
  }

  def mkTableIter[Row]
    (table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
    new StdTableIter[Row](table, scanId, direction, kernelInput)
  def unmkTableIter[Row](p: Rep[CursorIter[Row]]) = p match {
    case p: TableIter[Row] @unchecked =>
      Some((p.table, p.scanId, p.direction, p.kernelInput))
    case _ => None
  }

  case class StdIndexIter[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsIndexIter[Row](table, index, scanId, direction, kernelInput) {
  }

  def mkIndexIter[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
    new StdIndexIter[Row](table, index, scanId, direction, kernelInput)
  def unmkIndexIter[Row](p: Rep[CursorIter[Row]]) = p match {
    case p: IndexIter[Row] @unchecked =>
      Some((p.table, p.index, p.scanId, p.direction, p.kernelInput))
    case _ => None
  }

  case class StdSingletonIter[Row]
      (override val value: Rep[Row])(implicit eRow: Elem[Row])
    extends AbsSingletonIter[Row](value) {
  }

  def mkSingletonIter[Row]
    (value: Rep[Row])(implicit eRow: Elem[Row]): Rep[SingletonIter[Row]] =
    new StdSingletonIter[Row](value)
  def unmkSingletonIter[Row](p: Rep[AtMostOne[Row]]) = p match {
    case p: SingletonIter[Row] @unchecked =>
      Some((p.value))
    case _ => None
  }

  case class StdEmptyIter[Row]
      ()(implicit eRow: Elem[Row])
    extends AbsEmptyIter[Row]() {
  }

  def mkEmptyIter[Row]
    ()(implicit eRow: Elem[Row]): Rep[EmptyIter[Row]] =
    new StdEmptyIter[Row]()
  def unmkEmptyIter[Row](p: Rep[AtMostOne[Row]]) = p match {
    case p: EmptyIter[Row] @unchecked =>
      Some(())
    case _ => None
  }

  case class StdConditionalIter[Row]
      (override val condition: Rep[Boolean], override val baseIter: RIter[Row])(implicit eRow: Elem[Row])
    extends AbsConditionalIter[Row](condition, baseIter) {
  }

  def mkConditionalIter[Row]
    (condition: Rep[Boolean], baseIter: RIter[Row])(implicit eRow: Elem[Row]): Rep[ConditionalIter[Row]] =
    new StdConditionalIter[Row](condition, baseIter)
  def unmkConditionalIter[Row](p: Rep[Iter[Row]]) = p match {
    case p: ConditionalIter[Row] @unchecked =>
      Some((p.condition, p.baseIter))
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

  lazy val AtMostOne: Rep[AtMostOneCompanionAbs] = new AtMostOneCompanionAbs {
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

    object fromBeginning {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[_$2] forSome {type _$2}) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(fakeDep, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "fromBeginning" =>
          Some((receiver, fakeDep)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[_$2] forSome {type _$2}) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[_$2] forSome {type _$2}) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromKeyWhile {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp, Rep[Row => Boolean], Rep[_$3] forSome {type _$3}) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(keyValues, operation, takeWhilePred, fakeDep, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "fromKeyWhile" =>
          Some((receiver, keyValues, operation, takeWhilePred, fakeDep)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp, Rep[Row => Boolean], Rep[_$3] forSome {type _$3}) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp, Rep[Row => Boolean], Rep[_$3] forSome {type _$3}) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromKeyWhileTh {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any] => Unit], ComparisonOp, Rep[Row => Boolean], Rep[_$4] forSome {type _$4}) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(keyValues, operation, takeWhilePred, fakeDep, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "fromKeyWhileTh" =>
          Some((receiver, keyValues, operation, takeWhilePred, fakeDep)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[Array[Any] => Unit], ComparisonOp, Rep[Row => Boolean], Rep[_$4] forSome {type _$4}) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any] => Unit], ComparisonOp, Rep[Row => Boolean], Rep[_$4] forSome {type _$4}) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object uniqueByKey {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(keyValues, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "uniqueByKey" =>
          Some((receiver, keyValues)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[Array[Any]]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object uniqueByKeyTh {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any] => Unit]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(keyValues, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "uniqueByKeyTh" =>
          Some((receiver, keyValues)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[Array[Any] => Unit]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any] => Unit]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object uniqueValueByKey {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(keyValues, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "uniqueValueByKey" =>
          Some((receiver, keyValues)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[Array[Any]]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object AtMostOneMethods {
    object takeWhile {
      def unapply(d: Def[_]): Option[(Rep[AtMostOne[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[AtMostOneElem[_, _]] && method.getName == "takeWhile" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[AtMostOne[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AtMostOne[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceValue {
      def unapply(d: Def[_]): Option[(Rep[AtMostOne[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[AtMostOneElem[_, _]] && method.getName == "reduceValue" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[AtMostOne[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AtMostOne[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[AtMostOne[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[AtMostOneElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[AtMostOne[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AtMostOne[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object partialMapReduce {
      def unapply(d: Def[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[AtMostOneElem[_, _]] && method.getName == "partialMapReduce" =>
          Some((receiver, prefixComparator, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sort {
      def unapply(d: Def[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[AtMostOneElem[_, _]] && method.getName == "sort" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[AtMostOneElem[_, _]] && method.getName == "sortBy" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object partialSort {
      def unapply(d: Def[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, suffixComparator, _*), _) if receiver.elem.isInstanceOf[AtMostOneElem[_, _]] && method.getName == "partialSort" =>
          Some((receiver, prefixComparator, suffixComparator)).asInstanceOf[Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[AtMostOne[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  case class ExpTableIter[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsTableIter[Row](table, scanId, direction, kernelInput)

  object TableIterMethods {
    object byRowids {
      def unapply(d: Def[_]): Option[(Rep[TableIter[Row]], RIter[B], Rep[B => Rowid]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(iter, f, _*), _) if receiver.elem.isInstanceOf[TableIterElem[_]] && method.getName == "byRowids" =>
          Some((receiver, iter, f)).asInstanceOf[Option[(Rep[TableIter[Row]], RIter[B], Rep[B => Rowid]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[TableIter[Row]], RIter[B], Rep[B => Rowid]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object uniqueByRowid {
      def unapply(d: Def[_]): Option[(Rep[TableIter[Row]], Rep[Rowid]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(rowid, _*), _) if receiver.elem.isInstanceOf[TableIterElem[_]] && method.getName == "uniqueByRowid" =>
          Some((receiver, rowid)).asInstanceOf[Option[(Rep[TableIter[Row]], Rep[Rowid]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[TableIter[Row]], Rep[Rowid]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object uniqueByRowidTh {
      def unapply(d: Def[_]): Option[(Rep[TableIter[Row]], Rep[Thunk[Rowid]]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(rowid, _*), _) if receiver.elem.isInstanceOf[TableIterElem[_]] && method.getName == "uniqueByRowidTh" =>
          Some((receiver, rowid)).asInstanceOf[Option[(Rep[TableIter[Row]], Rep[Thunk[Rowid]]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[TableIter[Row]], Rep[Thunk[Rowid]]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object uniqueValueByRowid {
      def unapply(d: Def[_]): Option[(Rep[TableIter[Row]], Rep[Rowid]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(rowid, _*), _) if receiver.elem.isInstanceOf[TableIterElem[_]] && method.getName == "uniqueValueByRowid" =>
          Some((receiver, rowid)).asInstanceOf[Option[(Rep[TableIter[Row]], Rep[Rowid]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[TableIter[Row]], Rep[Rowid]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRowidWhile {
      def unapply(d: Def[_]): Option[(Rep[TableIter[Row]], Rep[Rowid], ComparisonOp, Rep[Row => Boolean], Rep[_$5] forSome {type _$5}) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(rowid, operation, takeWhilePred, fakeDep, _*), _) if receiver.elem.isInstanceOf[TableIterElem[_]] && method.getName == "fromRowidWhile" =>
          Some((receiver, rowid, operation, takeWhilePred, fakeDep)).asInstanceOf[Option[(Rep[TableIter[Row]], Rep[Rowid], ComparisonOp, Rep[Row => Boolean], Rep[_$5] forSome {type _$5}) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[TableIter[Row]], Rep[Rowid], ComparisonOp, Rep[Row => Boolean], Rep[_$5] forSome {type _$5}) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object fromRowidWhileTh {
      def unapply(d: Def[_]): Option[(Rep[TableIter[Row]], Rep[Thunk[Rowid]], ComparisonOp, Rep[Row => Boolean], Rep[_$6] forSome {type _$6}) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(rowid, operation, takeWhilePred, fakeDep, _*), _) if receiver.elem.isInstanceOf[TableIterElem[_]] && method.getName == "fromRowidWhileTh" =>
          Some((receiver, rowid, operation, takeWhilePred, fakeDep)).asInstanceOf[Option[(Rep[TableIter[Row]], Rep[Thunk[Rowid]], ComparisonOp, Rep[Row => Boolean], Rep[_$6] forSome {type _$6}) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[TableIter[Row]], Rep[Thunk[Rowid]], ComparisonOp, Rep[Row => Boolean], Rep[_$6] forSome {type _$6}) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkTableIter[Row]
    (table: Rep[Table], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[TableIter[Row]] =
    new ExpTableIter[Row](table, scanId, direction, kernelInput)
  def unmkTableIter[Row](p: Rep[CursorIter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TableIterElem[Row] @unchecked =>
      Some((p.asRep[TableIter[Row]].table, p.asRep[TableIter[Row]].scanId, p.asRep[TableIter[Row]].direction, p.asRep[TableIter[Row]].kernelInput))
    case _ =>
      None
  }

  case class ExpIndexIter[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsIndexIter[Row](table, index, scanId, direction, kernelInput)

  object IndexIterMethods {
  }

  def mkIndexIter[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int], direction: Rep[SortDirection], kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row]): Rep[IndexIter[Row]] =
    new ExpIndexIter[Row](table, index, scanId, direction, kernelInput)
  def unmkIndexIter[Row](p: Rep[CursorIter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexIterElem[Row] @unchecked =>
      Some((p.asRep[IndexIter[Row]].table, p.asRep[IndexIter[Row]].index, p.asRep[IndexIter[Row]].scanId, p.asRep[IndexIter[Row]].direction, p.asRep[IndexIter[Row]].kernelInput))
    case _ =>
      None
  }

  case class ExpSingletonIter[Row]
      (override val value: Rep[Row])(implicit eRow: Elem[Row])
    extends AbsSingletonIter[Row](value)

  object SingletonIterMethods {
    object map {
      def unapply(d: Def[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SingletonIter[Row]], Rep[Row => B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "flatMap" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SingletonIter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap0or1 {
      def unapply(d: Def[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "flatMap0or1" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SingletonIter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "filter" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[SingletonIter[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[SingletonIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[SingletonIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SingletonIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceValue {
      def unapply(d: Def[_]): Option[(Rep[SingletonIter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "reduceValue" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[SingletonIter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SingletonIter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "mapReduce" =>
          Some((receiver, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[SingletonIter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SingletonIter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object join {
      def unapply(d: Def[_]): Option[(Rep[SingletonIter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, cloneOther, _*), _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "join" =>
          Some((receiver, other, thisKey, otherKey, cloneOther)).asInstanceOf[Option[(Rep[SingletonIter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[SingletonIter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toArray {
      def unapply(d: Def[_]): Option[Rep[SingletonIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[SingletonIterElem[_]] && method.getName == "toArray" =>
          Some(receiver).asInstanceOf[Option[Rep[SingletonIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[SingletonIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkSingletonIter[Row]
    (value: Rep[Row])(implicit eRow: Elem[Row]): Rep[SingletonIter[Row]] =
    new ExpSingletonIter[Row](value)
  def unmkSingletonIter[Row](p: Rep[AtMostOne[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: SingletonIterElem[Row] @unchecked =>
      Some((p.asRep[SingletonIter[Row]].value))
    case _ =>
      None
  }

  case class ExpEmptyIter[Row]
      ()(implicit eRow: Elem[Row])
    extends AbsEmptyIter[Row]()

  object EmptyIterMethods {
    object map {
      def unapply(d: Def[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[EmptyIter[Row]], Rep[Row => B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "flatMap" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[EmptyIter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap0or1 {
      def unapply(d: Def[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "flatMap0or1" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[EmptyIter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "filter" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[EmptyIter[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[EmptyIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[EmptyIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[EmptyIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceValue {
      def unapply(d: Def[_]): Option[(Rep[EmptyIter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "reduceValue" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[EmptyIter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[EmptyIter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "mapReduce" =>
          Some((receiver, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[EmptyIter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[EmptyIter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object join {
      def unapply(d: Def[_]): Option[(Rep[EmptyIter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, cloneOther, _*), _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "join" =>
          Some((receiver, other, thisKey, otherKey, cloneOther)).asInstanceOf[Option[(Rep[EmptyIter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[EmptyIter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object toArray {
      def unapply(d: Def[_]): Option[Rep[EmptyIter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[EmptyIterElem[_]] && method.getName == "toArray" =>
          Some(receiver).asInstanceOf[Option[Rep[EmptyIter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[EmptyIter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkEmptyIter[Row]
    ()(implicit eRow: Elem[Row]): Rep[EmptyIter[Row]] =
    new ExpEmptyIter[Row]()
  def unmkEmptyIter[Row](p: Rep[AtMostOne[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: EmptyIterElem[Row] @unchecked =>
      Some(())
    case _ =>
      None
  }

  case class ExpConditionalIter[Row]
      (override val condition: Rep[Boolean], override val baseIter: RIter[Row])(implicit eRow: Elem[Row])
    extends AbsConditionalIter[Row](condition, baseIter)

  object ConditionalIterMethods {
  }

  def mkConditionalIter[Row]
    (condition: Rep[Boolean], baseIter: RIter[Row])(implicit eRow: Elem[Row]): Rep[ConditionalIter[Row]] =
    new ExpConditionalIter[Row](condition, baseIter)
  def unmkConditionalIter[Row](p: Rep[Iter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ConditionalIterElem[Row] @unchecked =>
      Some((p.asRep[ConditionalIter[Row]].condition, p.asRep[ConditionalIter[Row]].baseIter))
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

    object flatFlatMap {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row], Seq[Rep[_$1] forSome {type _$1}], RIter[B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(x, varsToGlobalize, resultIter, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "flatFlatMap" =>
          Some((receiver, x, varsToGlobalize, resultIter)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row], Seq[Rep[_$1] forSome {type _$1}], RIter[B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row], Seq[Rep[_$1] forSome {type _$1}], RIter[B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap0or1 {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "flatMap0or1" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Opt[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap0or1U {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Boolean]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "flatMap0or1U" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => Boolean]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Boolean]) forSome {type Row; type B}] = exp match {
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

    object toArray {
      def unapply(d: Def[_]): Option[Rep[Iter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "toArray" =>
          Some(receiver).asInstanceOf[Option[Rep[Iter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Iter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

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

    object single {
      def unapply(d: Def[_]): Option[Rep[Row] forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(value, _*), _) if receiver.elem == IterCompanionElem && method.getName == "single" =>
          Some(value).asInstanceOf[Option[Rep[Row] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Row] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object singleIf {
      def unapply(d: Def[_]): Option[(Rep[Boolean], Rep[Row]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(condition, value, _*), _) if receiver.elem == IterCompanionElem && method.getName == "singleIf" =>
          Some((condition, value)).asInstanceOf[Option[(Rep[Boolean], Rep[Row]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Boolean], Rep[Row]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Iters_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAO1YTWwbRRQeO3Yc20kaglqCREQILtACdkSFihQhlMYOuHV+lE1bSKui8e443XZ2drM7TmwO5VYJuCHEAYlDEYhLhIR6QSBxASSEUA9cwxVxaKmqHqiEBOLN7I/Xweu2+ZF6wIfR7Mzb9/O9772d8cYfKOnY6ClHxRSzvEE4zityPuXwnFJiXOfNWVOrU1IktXOvffLXGeOdR+JoaBn1nsdO0aHLKO1OSg0rmCtcq6A0ZipxuGk7HD1RkRYKqkkpUblusoJuGHWOq5QUKrrDJysoUTW15iq6hGIVNKSaTLUJJ8o0xY5DHG+9jwiP9OA5LZ+b81bLBiuIKAqhKJZsrHNwH2wMufKLxFKazGRNg6NBz7V5S7gFMlnSsCCGsmFRaaanglK6YZk2962mwMJ5U/MfEwzDAhquXMBruABWVwoKt3W2IpRZWL2IV8gciAjxBMTgEFpbalrEU551uNZmr2EhhCArL0jH8i3M8gFmeYFZTiG2jqn+FhabC7bZaCL3F+tBqGGBiufuosLXQEpMy717Vj1zR8kacfFyQ7iSkg71gqLHIxgi0wPY/rj4vnP71StH4yizjDK6M1V1uI1VHqaBB1cWM2Zy6XOAILZXIIPjURmUVqZAZgtN0qppWJiBJg/LfkgU1VWdC2Gx1u+lJwL7FLeILxprWLEg3rGIeCWXpjGlC9cfff7gjdLrcRRvN5EGlQoUg+0r5SgzXbcd0y5zYnsGxLiPo55Fc13iLIZ0ozWmurgQgPH09ZvaDxPobDyA0LN4b1kDFcMvffT1QbLwRRz1LUuSz1C8IvMnMCoSR11GfeYasd311BqmYtYxhymN1HCdcg/ZMCQ9AAlHY5H1aRGB16TkfcwPP+tSd85kJDezkPtT+emDDcFMG/W7O27B/qMf/XtzsMYlaTlKSm638CVWO+LJJbEfwlxujASGxTDKQV7FrKx10GOjJ6OoYZEFWzegPa2RF7/75uStb+eSkh3DHjKnMK0TtzN4wLRAEr7HJsBSmfFujqU13XYrOTrGAQV6VdGXu2us2YvEZoSWmVXn0UqzJ1pSnSkr1Unx/aFXR2JbzCUIkN7XmihRYtxDTUDgMnGihFp2RDZGo7MBBH/jhpYfuTm6Hke9x1GyBtx1KihZNetM8zsGfGk4afBj/lqsnbvQIbCNjeADtIahY0JH4+iAz+c612nhlLfushh+Y0g6KkMJRQUuH/BcFu/ly8zVyHPPfrWxrl87NCOZHEpYNtYO0Ha6yX+yspUEu1Q3SZ1ppNFFTVnsb7/8xHD4//roVB8S2a31IcaZrnwUw4kubEtP8VnT4fOM7CLZ1kQnjMYyWvfeYjigQGumhJvswcEx2Ntr/pQMizd3P+7E7vYpOPExTe9S34eAXjVMHWjQqWOmSQlm3Yq7r4odErgoOtTi/Ti8tznZN+0Hi+mOMhNCoNux0L8ofXn58v5bn775sDzM91V1bmArN3EfR3kx79/bozragiC4vX3iRVQcQJVxz5qKaZCHxm/r5668x+VxO9Zov/DNVy/A52RS6nlM6tlEIUWdShlOfAPCx+kwMG4WLc/9nZXiJto+IpEuh04fD6B3rS67w1oR468tHb8DFfIRVVMkKsU20cQFnhiEefVw5MNXTh8fOX1S0qVfk0LuTnAV6vx3yCy2JuXl/Zkul3cQysmOLSZHvn/5l7d//vyz4OSY8uJLSky5x2KWd1ZpEM54RDiKV3ZQUJfufDx3+NrV3+QtJiMKGC5jLPgbJHx7aU9cn7RbdGgrWeIT6FpRVmkoqZB4UeieWzDGMmIY/BdpphYRmhIAAA=="
}
}

