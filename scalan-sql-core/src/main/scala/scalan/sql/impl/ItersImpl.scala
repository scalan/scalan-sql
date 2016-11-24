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

  // single proxy for each type family
  implicit def proxyAtMostOne[Row](p: Rep[AtMostOne[Row]]): AtMostOne[Row] = {
    proxyOps[AtMostOne[Row]](p)(scala.reflect.classTag[AtMostOne[Row]])
  }
  // familyElem
  class AtMostOneElem[Row, To <: AtMostOne[Row]](implicit _eRow: Elem[Row])
    extends IterElem[Row, To] {
    override def eRow = _eRow
    override lazy val parent: Option[Elem[_]] = Some(iterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)
    override def isEntityType = true
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
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new SingletonIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[SingletonIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
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
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new EmptyIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[EmptyIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
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
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

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
    def isEntityType = true
    def getDefaultRep = reifyObject(new ConditionalIterIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[ConditionalIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
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

    object fromKeyWhile {
      def unapply(d: Def[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp, Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(keyValues, operation, takeWhilePred, _*), _) if receiver.elem.isInstanceOf[CursorIterElem[_, _]] && method.getName == "fromKeyWhile" =>
          Some((receiver, keyValues, operation, takeWhilePred)).asInstanceOf[Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp, Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[CursorIter[Row]], Rep[Array[Any]], ComparisonOp, Rep[Row => Boolean]) forSome {type Row}] = exp match {
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
      (override val table: Rep[Table], override val scanId: Rep[Int], override val direction: Rep[SortDirection], override val fakeDep: Rep[Unit], override val kernelInput: Rep[KernelInput])(implicit eRow: Elem[Row])
    extends AbsTableIter[Row](table, scanId, direction, fakeDep, kernelInput)

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
  val dump = "H4sIAAAAAAAAAN1YTWwbRRQeO3Ztx0kbAlWDRNUQXCgV2BEVKihCKI1dcHGcKJtQlFZB491xus3s7GZnHGwO5VYJuCEEEhKHIhCXClRxA4kLICGEOHDlHC4FVHqgEhKIN7M/ttOs21qJFOHDaHbm7fv53vvezvjq7yjJXfQo1zHFLG8RgfOamk9zkdNKTJiiNWsbDUqKpL7y4kd/n7PePBRHI8to3wXMi5wuo4w3KTWdcK4Jo4IymOmEC9vlAj1cURYKuk0p0YVps4JpWQ2Ba5QUKiYXUxWUqNlGax1dQrEKGtFtprtEEG2GYs4J99fTRHpkhs8Z9dyac9o2WEFGUeiIYtHFpgD3wcaIJ79AHK3FbNayBNrvuzbnSLdAJkuaDsRQthyqzAxUUMq0HNsVgdUUWLhgG8FjgmFYQKOVi3gDF8DqakETrslWpTIH62t4lVRBRIonIAZOaH2x5RBfeZYLo8te00EIQVaeUo7l25jlQ8zyErOcRlwTU/N1LDfnXbvZQt4vNoBQ0wEVT9xBRaCBlJiRe+u8fu6WlrXi8uWmdCWlHNoHio5EVIhKD2D7/cI7/OYLV07G0eAyGjT5dI0LF+uiswx8uLKYMVson0MEsbsKGZyIyqCyMg0yW8oko9uWgxlo8rEcgkRRUzeFFJZrQ356IrBPCYcEorGmEwvjHY+IV9XSDKZ0/vqDTx79rfRKHMW7TWRApQZkcAOlAg3ONFxuu2VBXN+AHA8INLBgv6ZwlkOm2R5TPVwIwXjs+h/Gd5PofDyE0Ld4d1kDFaPPfPDlUTL/WRyll1WRn6Z4VeVPYlQkXF9GaXuDuN56agNTOds2hymD1HGDCh/ZTkgGABKBxiP56RCJ15Sq+1gQftYr3arNSO70fO4v7Yd3r8rKdNGQt+MR9l/z5D+/7K8LVbQCJVVtt/ElTjfiyUW534G52hgLDcvhsAB5HbOysY0eFz0SVRoOmXdNC9rTBnn6m6+WbnxdTarqGPWReRnTBvE6gw9MGyTpe2wSLJWZ6OVYxjBdj8nRMQ5r0KuKgdwdY03V8Rrk2olQ+LhA8WMwJJaY2dO17BpxGaFl5jREtHPZl9pS25e+UqfED3a8OhbbYi5BgDyB1kSJEusuuAUAqgKQVGzbkVk9HJ1VIAo7pM2+//mRlThKnkHJOnCAV1CyZjeYEXQe+GIJ0hSngrVYNweg02AXWwFFvT49jpQTys3bPFai2Vh3VP20ktug3Jq5HSJN0mQGafZQU5b7/XNPDsf3DDnk8Oz/gA8qKVv5IMdqvzWamRazNhdzjOxgiW7I5hkNY7Tu3YVvWINuTomw2Z6AMNzb7aopWY5o7WjIiZ3taXA0ZIbZoxfAJy1Zx5TDySR1yrYpwawXm9M1zEnoouxmC/fi8O6m48BMECym/SalI/heR8fgMnXt8uWDNz5+9X514E/XTGFhJzd5D8d9OR/a3eM82gIeuN1/zUXwDKAa9M6jmm2R+yZumitX3hbqSB5rdl8K52oX4aszpfQ8pPRsog5F2xEYToXD0seZTmC8BDq++30TcBP1D0aktx3nk73lWLuj9k8OOf7afv1PyH0+giZFolPsEkPe6olFmE+AE+89f/bM2NklVR9DhhLydsL70fb/kcxiZ0rd6I/1uNGDUE41Zjk58e1zP7/x46efqItRGw95DJNwCr9sWZ6v0zCciYhwNJ9nwKBLtz6sHv/pi011tRmUjIUbGgv/G+m80nTnLK3sFjlt50l+6Twr2jrtgBtyLpntuwVj7AE5jP0HmecA7K8SAAA="
}
}

