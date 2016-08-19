package scalan.sql

import java.lang.reflect.Method
import scalan._
import scalan.common.Lazy
import scala.reflect.runtime.universe._
import scalan.sql.parser.SqlAST.{Index, Table}
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait RelationsAbs extends Scalan with Relations {
  self: RelationsDsl with ScalanSql =>

  // single proxy for each type family
  implicit def proxyRelation[Row](p: Rep[Relation[Row]]): Relation[Row] = {
    proxyOps[Relation[Row]](p)(scala.reflect.classTag[Relation[Row]])
  }

  // familyElem
  class RelationElem[Row, To <: Relation[Row]](implicit _eRow: Elem[Row])
    extends EntityElem[To] {
    def eRow = _eRow
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("Row" -> eRow)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[Relation[Row]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Relation[Row]] => convertRelation(x) }
      tryConvert(element[Relation[Row]], this, x, conv)
    }

    def convertRelation(x: Rep[Relation[Row]]): Rep[To] = {
      x.selfType1 match {
        case _: RelationElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have RelationElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def relationElement[Row](implicit eRow: Elem[Row]): Elem[Relation[Row]] =
    cachedElem[RelationElem[Row, Relation[Row]]](eRow)

  implicit case object RelationCompanionElem extends CompanionElem[RelationCompanionAbs] {
    lazy val tag = weakTypeTag[RelationCompanionAbs]
    protected def getDefaultRep = Relation
  }

  abstract class RelationCompanionAbs extends CompanionDef[RelationCompanionAbs] {
    def selfType = RelationCompanionElem
    override def toString = "Relation"
  }
  def Relation: Rep[RelationCompanionAbs]
  implicit def proxyRelationCompanionAbs(p: Rep[RelationCompanionAbs]): RelationCompanionAbs =
    proxyOps[RelationCompanionAbs](p)

  // single proxy for each type family
  implicit def proxyPhysicalRelation[Row](p: Rep[PhysicalRelation[Row]]): PhysicalRelation[Row] = {
    proxyOps[PhysicalRelation[Row]](p)(scala.reflect.classTag[PhysicalRelation[Row]])
  }
  // familyElem
  class PhysicalRelationElem[Row, To <: PhysicalRelation[Row]](implicit _eRow: Elem[Row])
    extends RelationElem[Row, To] {
    override def eRow = _eRow
    override lazy val parent: Option[Elem[_]] = Some(relationElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[PhysicalRelation[Row]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[PhysicalRelation[Row]] => convertPhysicalRelation(x) }
      tryConvert(element[PhysicalRelation[Row]], this, x, conv)
    }

    def convertPhysicalRelation(x: Rep[PhysicalRelation[Row]]): Rep[To] = {
      x.selfType1 match {
        case _: PhysicalRelationElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have PhysicalRelationElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def physicalRelationElement[Row](implicit eRow: Elem[Row]): Elem[PhysicalRelation[Row]] =
    cachedElem[PhysicalRelationElem[Row, PhysicalRelation[Row]]](eRow)

  implicit case object PhysicalRelationCompanionElem extends CompanionElem[PhysicalRelationCompanionAbs] {
    lazy val tag = weakTypeTag[PhysicalRelationCompanionAbs]
    protected def getDefaultRep = PhysicalRelation
  }

  abstract class PhysicalRelationCompanionAbs extends CompanionDef[PhysicalRelationCompanionAbs] {
    def selfType = PhysicalRelationCompanionElem
    override def toString = "PhysicalRelation"
  }
  def PhysicalRelation: Rep[PhysicalRelationCompanionAbs]
  implicit def proxyPhysicalRelationCompanionAbs(p: Rep[PhysicalRelationCompanionAbs]): PhysicalRelationCompanionAbs =
    proxyOps[PhysicalRelationCompanionAbs](p)

  abstract class AbsIterBasedRelation[Row]
      (iter: RIter[Row])(implicit eRow: Elem[Row])
    extends IterBasedRelation[Row](iter) with Def[IterBasedRelation[Row]] {
    lazy val selfType = element[IterBasedRelation[Row]]
  }
  // elem for concrete class
  class IterBasedRelationElem[Row](val iso: Iso[IterBasedRelationData[Row], IterBasedRelation[Row]])(implicit override val eRow: Elem[Row])
    extends RelationElem[Row, IterBasedRelation[Row]]
    with ConcreteElem[IterBasedRelationData[Row], IterBasedRelation[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(relationElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertRelation(x: Rep[Relation[Row]]) = IterBasedRelation(x.iter)
    override def getDefaultRep = IterBasedRelation(element[Iter[Row]].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IterBasedRelation[Row]]
    }
  }

  // state representation type
  type IterBasedRelationData[Row] = Iter[Row]

  // 3) Iso for concrete class
  class IterBasedRelationIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[IterBasedRelationData[Row], IterBasedRelation[Row]] with Def[IterBasedRelationIso[Row]] {
    override def from(p: Rep[IterBasedRelation[Row]]) =
      p.iter
    override def to(p: Rep[Iter[Row]]) = {
      val iter = p
      IterBasedRelation(iter)
    }
    lazy val eFrom = element[Iter[Row]]
    lazy val eTo = new IterBasedRelationElem[Row](self)
    lazy val selfType = new IterBasedRelationIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class IterBasedRelationIsoElem[Row](eRow: Elem[Row]) extends Elem[IterBasedRelationIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IterBasedRelationIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IterBasedRelationIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class IterBasedRelationCompanionAbs extends CompanionDef[IterBasedRelationCompanionAbs] {
    def selfType = IterBasedRelationCompanionElem
    override def toString = "IterBasedRelation"

    @scalan.OverloadId("fromFields")
    def apply[Row](iter: RIter[Row])(implicit eRow: Elem[Row]): Rep[IterBasedRelation[Row]] =
      mkIterBasedRelation(iter)

    def unapply[Row](p: Rep[Relation[Row]]) = unmkIterBasedRelation(p)
  }
  lazy val IterBasedRelationRep: Rep[IterBasedRelationCompanionAbs] = new IterBasedRelationCompanionAbs
  lazy val IterBasedRelation: IterBasedRelationCompanionAbs = proxyIterBasedRelationCompanion(IterBasedRelationRep)
  implicit def proxyIterBasedRelationCompanion(p: Rep[IterBasedRelationCompanionAbs]): IterBasedRelationCompanionAbs = {
    proxyOps[IterBasedRelationCompanionAbs](p)
  }

  implicit case object IterBasedRelationCompanionElem extends CompanionElem[IterBasedRelationCompanionAbs] {
    lazy val tag = weakTypeTag[IterBasedRelationCompanionAbs]
    protected def getDefaultRep = IterBasedRelation
  }

  implicit def proxyIterBasedRelation[Row](p: Rep[IterBasedRelation[Row]]): IterBasedRelation[Row] =
    proxyOps[IterBasedRelation[Row]](p)

  implicit class ExtendedIterBasedRelation[Row](p: Rep[IterBasedRelation[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[IterBasedRelationData[Row]] = isoIterBasedRelation(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIterBasedRelation[Row](implicit eRow: Elem[Row]): Iso[IterBasedRelationData[Row], IterBasedRelation[Row]] =
    reifyObject(new IterBasedRelationIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkIterBasedRelation[Row](iter: RIter[Row])(implicit eRow: Elem[Row]): Rep[IterBasedRelation[Row]]
  def unmkIterBasedRelation[Row](p: Rep[Relation[Row]]): Option[(Rep[Iter[Row]])]

  abstract class AbsTableRelation[Row]
      (table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends TableRelation[Row](table, scanId) with Def[TableRelation[Row]] {
    lazy val selfType = element[TableRelation[Row]]
  }
  // elem for concrete class
  class TableRelationElem[Row](val iso: Iso[TableRelationData[Row], TableRelation[Row]])(implicit override val eRow: Elem[Row])
    extends PhysicalRelationElem[Row, TableRelation[Row]]
    with ConcreteElem[TableRelationData[Row], TableRelation[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(physicalRelationElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertPhysicalRelation(x: Rep[PhysicalRelation[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from PhysicalRelation to TableRelation: missing fields List(table, scanId)")
    override def getDefaultRep = TableRelation(element[Table].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableRelation[Row]]
    }
  }

  // state representation type
  type TableRelationData[Row] = (Table, Int)

  // 3) Iso for concrete class
  class TableRelationIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[TableRelationData[Row], TableRelation[Row]] with Def[TableRelationIso[Row]] {
    override def from(p: Rep[TableRelation[Row]]) =
      (p.table, p.scanId)
    override def to(p: Rep[(Table, Int)]) = {
      val Pair(table, scanId) = p
      TableRelation(table, scanId)
    }
    lazy val eFrom = pairElement(element[Table], element[Int])
    lazy val eTo = new TableRelationElem[Row](self)
    lazy val selfType = new TableRelationIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class TableRelationIsoElem[Row](eRow: Elem[Row]) extends Elem[TableRelationIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new TableRelationIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableRelationIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class TableRelationCompanionAbs extends CompanionDef[TableRelationCompanionAbs] {
    def selfType = TableRelationCompanionElem
    override def toString = "TableRelation"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[TableRelationData[Row]])(implicit eRow: Elem[Row]): Rep[TableRelation[Row]] =
      isoTableRelation(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableRelation[Row]] =
      mkTableRelation(table, scanId)

    def unapply[Row](p: Rep[PhysicalRelation[Row]]) = unmkTableRelation(p)
  }
  lazy val TableRelationRep: Rep[TableRelationCompanionAbs] = new TableRelationCompanionAbs
  lazy val TableRelation: TableRelationCompanionAbs = proxyTableRelationCompanion(TableRelationRep)
  implicit def proxyTableRelationCompanion(p: Rep[TableRelationCompanionAbs]): TableRelationCompanionAbs = {
    proxyOps[TableRelationCompanionAbs](p)
  }

  implicit case object TableRelationCompanionElem extends CompanionElem[TableRelationCompanionAbs] {
    lazy val tag = weakTypeTag[TableRelationCompanionAbs]
    protected def getDefaultRep = TableRelation
  }

  implicit def proxyTableRelation[Row](p: Rep[TableRelation[Row]]): TableRelation[Row] =
    proxyOps[TableRelation[Row]](p)

  implicit class ExtendedTableRelation[Row](p: Rep[TableRelation[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[TableRelationData[Row]] = isoTableRelation(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoTableRelation[Row](implicit eRow: Elem[Row]): Iso[TableRelationData[Row], TableRelation[Row]] =
    reifyObject(new TableRelationIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkTableRelation[Row](table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableRelation[Row]]
  def unmkTableRelation[Row](p: Rep[PhysicalRelation[Row]]): Option[(Rep[Table], Rep[Int])]

  abstract class AbsIndexRelation[Row]
      (table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends IndexRelation[Row](table, index, scanId) with Def[IndexRelation[Row]] {
    lazy val selfType = element[IndexRelation[Row]]
  }
  // elem for concrete class
  class IndexRelationElem[Row](val iso: Iso[IndexRelationData[Row], IndexRelation[Row]])(implicit override val eRow: Elem[Row])
    extends PhysicalRelationElem[Row, IndexRelation[Row]]
    with ConcreteElem[IndexRelationData[Row], IndexRelation[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(physicalRelationElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertPhysicalRelation(x: Rep[PhysicalRelation[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from PhysicalRelation to IndexRelation: missing fields List(table, index, scanId)")
    override def getDefaultRep = IndexRelation(element[Table].defaultRepValue, element[Index].defaultRepValue, 0)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexRelation[Row]]
    }
  }

  // state representation type
  type IndexRelationData[Row] = (Table, (Index, Int))

  // 3) Iso for concrete class
  class IndexRelationIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[IndexRelationData[Row], IndexRelation[Row]] with Def[IndexRelationIso[Row]] {
    override def from(p: Rep[IndexRelation[Row]]) =
      (p.table, p.index, p.scanId)
    override def to(p: Rep[(Table, (Index, Int))]) = {
      val Pair(table, Pair(index, scanId)) = p
      IndexRelation(table, index, scanId)
    }
    lazy val eFrom = pairElement(element[Table], pairElement(element[Index], element[Int]))
    lazy val eTo = new IndexRelationElem[Row](self)
    lazy val selfType = new IndexRelationIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class IndexRelationIsoElem[Row](eRow: Elem[Row]) extends Elem[IndexRelationIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IndexRelationIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexRelationIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class IndexRelationCompanionAbs extends CompanionDef[IndexRelationCompanionAbs] {
    def selfType = IndexRelationCompanionElem
    override def toString = "IndexRelation"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[IndexRelationData[Row]])(implicit eRow: Elem[Row]): Rep[IndexRelation[Row]] =
      isoIndexRelation(eRow).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexRelation[Row]] =
      mkIndexRelation(table, index, scanId)

    def unapply[Row](p: Rep[PhysicalRelation[Row]]) = unmkIndexRelation(p)
  }
  lazy val IndexRelationRep: Rep[IndexRelationCompanionAbs] = new IndexRelationCompanionAbs
  lazy val IndexRelation: IndexRelationCompanionAbs = proxyIndexRelationCompanion(IndexRelationRep)
  implicit def proxyIndexRelationCompanion(p: Rep[IndexRelationCompanionAbs]): IndexRelationCompanionAbs = {
    proxyOps[IndexRelationCompanionAbs](p)
  }

  implicit case object IndexRelationCompanionElem extends CompanionElem[IndexRelationCompanionAbs] {
    lazy val tag = weakTypeTag[IndexRelationCompanionAbs]
    protected def getDefaultRep = IndexRelation
  }

  implicit def proxyIndexRelation[Row](p: Rep[IndexRelation[Row]]): IndexRelation[Row] =
    proxyOps[IndexRelation[Row]](p)

  implicit class ExtendedIndexRelation[Row](p: Rep[IndexRelation[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[IndexRelationData[Row]] = isoIndexRelation(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexRelation[Row](implicit eRow: Elem[Row]): Iso[IndexRelationData[Row], IndexRelation[Row]] =
    reifyObject(new IndexRelationIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkIndexRelation[Row](table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexRelation[Row]]
  def unmkIndexRelation[Row](p: Rep[PhysicalRelation[Row]]): Option[(Rep[Table], Rep[Index], Rep[Int])]

  abstract class AbsWrapRelation[Row, Row2]
      (env: RRelation[Row2], f: RFunc[Iter[Row2], Iter[Row]])(implicit eRow: Elem[Row], eRow2: Elem[Row2])
    extends WrapRelation[Row, Row2](env, f) with Def[WrapRelation[Row, Row2]] {
    lazy val selfType = element[WrapRelation[Row, Row2]]
  }
  // elem for concrete class
  class WrapRelationElem[Row, Row2](val iso: Iso[WrapRelationData[Row, Row2], WrapRelation[Row, Row2]])(implicit override val eRow: Elem[Row], val eRow2: Elem[Row2])
    extends RelationElem[Row, WrapRelation[Row, Row2]]
    with ConcreteElem[WrapRelationData[Row, Row2], WrapRelation[Row, Row2]] {
    override lazy val parent: Option[Elem[_]] = Some(relationElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow, "Row2" -> eRow2)

    override def convertRelation(x: Rep[Relation[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Relation to WrapRelation: missing fields List(env, f)")
    override def getDefaultRep = WrapRelation(element[Relation[Row2]].defaultRepValue, constFun[Iter[Row2], Iter[Row]](element[Iter[Row]].defaultRepValue))
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      implicit val tagRow2 = eRow2.tag
      weakTypeTag[WrapRelation[Row, Row2]]
    }
  }

  // state representation type
  type WrapRelationData[Row, Row2] = (Relation[Row2], Iter[Row2] => Iter[Row])

  // 3) Iso for concrete class
  class WrapRelationIso[Row, Row2](implicit eRow: Elem[Row], eRow2: Elem[Row2])
    extends EntityIso[WrapRelationData[Row, Row2], WrapRelation[Row, Row2]] with Def[WrapRelationIso[Row, Row2]] {
    override def from(p: Rep[WrapRelation[Row, Row2]]) =
      (p.env, p.f)
    override def to(p: Rep[(Relation[Row2], Iter[Row2] => Iter[Row])]) = {
      val Pair(env, f) = p
      WrapRelation(env, f)
    }
    lazy val eFrom = pairElement(element[Relation[Row2]], element[Iter[Row2] => Iter[Row]])
    lazy val eTo = new WrapRelationElem[Row, Row2](self)
    lazy val selfType = new WrapRelationIsoElem[Row, Row2](eRow, eRow2)
    def productArity = 2
    def productElement(n: Int) = n match {
      case 0 => eRow
      case 1 => eRow2
    }
  }
  case class WrapRelationIsoElem[Row, Row2](eRow: Elem[Row], eRow2: Elem[Row2]) extends Elem[WrapRelationIso[Row, Row2]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new WrapRelationIso[Row, Row2]()(eRow, eRow2))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      implicit val tagRow2 = eRow2.tag
      weakTypeTag[WrapRelationIso[Row, Row2]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow, "Row2" -> eRow2)
  }
  // 4) constructor and deconstructor
  class WrapRelationCompanionAbs extends CompanionDef[WrapRelationCompanionAbs] {
    def selfType = WrapRelationCompanionElem
    override def toString = "WrapRelation"
    @scalan.OverloadId("fromData")
    def apply[Row, Row2](p: Rep[WrapRelationData[Row, Row2]])(implicit eRow: Elem[Row], eRow2: Elem[Row2]): Rep[WrapRelation[Row, Row2]] =
      isoWrapRelation(eRow, eRow2).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row, Row2](env: RRelation[Row2], f: RFunc[Iter[Row2], Iter[Row]])(implicit eRow: Elem[Row], eRow2: Elem[Row2]): Rep[WrapRelation[Row, Row2]] =
      mkWrapRelation(env, f)

    def unapply[Row, Row2](p: Rep[Relation[Row]]) = unmkWrapRelation(p)
  }
  lazy val WrapRelationRep: Rep[WrapRelationCompanionAbs] = new WrapRelationCompanionAbs
  lazy val WrapRelation: WrapRelationCompanionAbs = proxyWrapRelationCompanion(WrapRelationRep)
  implicit def proxyWrapRelationCompanion(p: Rep[WrapRelationCompanionAbs]): WrapRelationCompanionAbs = {
    proxyOps[WrapRelationCompanionAbs](p)
  }

  implicit case object WrapRelationCompanionElem extends CompanionElem[WrapRelationCompanionAbs] {
    lazy val tag = weakTypeTag[WrapRelationCompanionAbs]
    protected def getDefaultRep = WrapRelation
  }

  implicit def proxyWrapRelation[Row, Row2](p: Rep[WrapRelation[Row, Row2]]): WrapRelation[Row, Row2] =
    proxyOps[WrapRelation[Row, Row2]](p)

  implicit class ExtendedWrapRelation[Row, Row2](p: Rep[WrapRelation[Row, Row2]])(implicit eRow: Elem[Row], eRow2: Elem[Row2]) {
    def toData: Rep[WrapRelationData[Row, Row2]] = isoWrapRelation(eRow, eRow2).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoWrapRelation[Row, Row2](implicit eRow: Elem[Row], eRow2: Elem[Row2]): Iso[WrapRelationData[Row, Row2], WrapRelation[Row, Row2]] =
    reifyObject(new WrapRelationIso[Row, Row2]()(eRow, eRow2))

  // 6) smart constructor and deconstructor
  def mkWrapRelation[Row, Row2](env: RRelation[Row2], f: RFunc[Iter[Row2], Iter[Row]])(implicit eRow: Elem[Row], eRow2: Elem[Row2]): Rep[WrapRelation[Row, Row2]]
  def unmkWrapRelation[Row, Row2](p: Rep[Relation[Row]]): Option[(Rep[Relation[Row2]], Rep[Iter[Row2] => Iter[Row]])]

  abstract class AbsWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct]
      (env: Rep[EnvR], f: RFunc[EnvI, Iter[Row]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI])
    extends WrapStructRelation[Row, EnvR, EnvI](env, f) with Def[WrapStructRelation[Row, EnvR, EnvI]] {
    lazy val selfType = element[WrapStructRelation[Row, EnvR, EnvI]]
  }
  // elem for concrete class
  class WrapStructRelationElem[Row, EnvR <: Struct, EnvI <: Struct](val iso: Iso[WrapStructRelationData[Row, EnvR, EnvI], WrapStructRelation[Row, EnvR, EnvI]])(implicit override val eRow: Elem[Row], val eEnvR: Elem[EnvR], val eEnvI: Elem[EnvI])
    extends RelationElem[Row, WrapStructRelation[Row, EnvR, EnvI]]
    with ConcreteElem[WrapStructRelationData[Row, EnvR, EnvI], WrapStructRelation[Row, EnvR, EnvI]] {
    override lazy val parent: Option[Elem[_]] = Some(relationElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow, "EnvR" -> eEnvR, "EnvI" -> eEnvI)

    override def convertRelation(x: Rep[Relation[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Relation to WrapStructRelation: missing fields List(env, f)")
    override def getDefaultRep = WrapStructRelation(element[EnvR].defaultRepValue, constFun[EnvI, Iter[Row]](element[Iter[Row]].defaultRepValue))
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      implicit val tagEnvR = eEnvR.tag
      implicit val tagEnvI = eEnvI.tag
      weakTypeTag[WrapStructRelation[Row, EnvR, EnvI]]
    }
  }

  // state representation type
  type WrapStructRelationData[Row, EnvR <: Struct, EnvI <: Struct] = (EnvR, EnvI => Iter[Row])

  // 3) Iso for concrete class
  class WrapStructRelationIso[Row, EnvR <: Struct, EnvI <: Struct](implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI])
    extends EntityIso[WrapStructRelationData[Row, EnvR, EnvI], WrapStructRelation[Row, EnvR, EnvI]] with Def[WrapStructRelationIso[Row, EnvR, EnvI]] {
    override def from(p: Rep[WrapStructRelation[Row, EnvR, EnvI]]) =
      (p.env, p.f)
    override def to(p: Rep[(EnvR, EnvI => Iter[Row])]) = {
      val Pair(env, f) = p
      WrapStructRelation(env, f)
    }
    lazy val eFrom = pairElement(element[EnvR], element[EnvI => Iter[Row]])
    lazy val eTo = new WrapStructRelationElem[Row, EnvR, EnvI](self)
    lazy val selfType = new WrapStructRelationIsoElem[Row, EnvR, EnvI](eRow, eEnvR, eEnvI)
    def productArity = 3
    def productElement(n: Int) = n match {
      case 0 => eRow
      case 1 => eEnvR
      case 2 => eEnvI
    }
  }
  case class WrapStructRelationIsoElem[Row, EnvR <: Struct, EnvI <: Struct](eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]) extends Elem[WrapStructRelationIso[Row, EnvR, EnvI]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new WrapStructRelationIso[Row, EnvR, EnvI]()(eRow, eEnvR, eEnvI))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      implicit val tagEnvR = eEnvR.tag
      implicit val tagEnvI = eEnvI.tag
      weakTypeTag[WrapStructRelationIso[Row, EnvR, EnvI]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow, "EnvR" -> eEnvR, "EnvI" -> eEnvI)
  }
  // 4) constructor and deconstructor
  class WrapStructRelationCompanionAbs extends CompanionDef[WrapStructRelationCompanionAbs] {
    def selfType = WrapStructRelationCompanionElem
    override def toString = "WrapStructRelation"
    @scalan.OverloadId("fromData")
    def apply[Row, EnvR <: Struct, EnvI <: Struct](p: Rep[WrapStructRelationData[Row, EnvR, EnvI]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]): Rep[WrapStructRelation[Row, EnvR, EnvI]] =
      isoWrapStructRelation(eRow, eEnvR, eEnvI).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row, EnvR <: Struct, EnvI <: Struct](env: Rep[EnvR], f: RFunc[EnvI, Iter[Row]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]): Rep[WrapStructRelation[Row, EnvR, EnvI]] =
      mkWrapStructRelation(env, f)

    def unapply[Row, EnvR <: Struct, EnvI <: Struct](p: Rep[Relation[Row]]) = unmkWrapStructRelation(p)
  }
  lazy val WrapStructRelationRep: Rep[WrapStructRelationCompanionAbs] = new WrapStructRelationCompanionAbs
  lazy val WrapStructRelation: WrapStructRelationCompanionAbs = proxyWrapStructRelationCompanion(WrapStructRelationRep)
  implicit def proxyWrapStructRelationCompanion(p: Rep[WrapStructRelationCompanionAbs]): WrapStructRelationCompanionAbs = {
    proxyOps[WrapStructRelationCompanionAbs](p)
  }

  implicit case object WrapStructRelationCompanionElem extends CompanionElem[WrapStructRelationCompanionAbs] {
    lazy val tag = weakTypeTag[WrapStructRelationCompanionAbs]
    protected def getDefaultRep = WrapStructRelation
  }

  implicit def proxyWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](p: Rep[WrapStructRelation[Row, EnvR, EnvI]]): WrapStructRelation[Row, EnvR, EnvI] =
    proxyOps[WrapStructRelation[Row, EnvR, EnvI]](p)

  implicit class ExtendedWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](p: Rep[WrapStructRelation[Row, EnvR, EnvI]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]) {
    def toData: Rep[WrapStructRelationData[Row, EnvR, EnvI]] = isoWrapStructRelation(eRow, eEnvR, eEnvI).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]): Iso[WrapStructRelationData[Row, EnvR, EnvI], WrapStructRelation[Row, EnvR, EnvI]] =
    reifyObject(new WrapStructRelationIso[Row, EnvR, EnvI]()(eRow, eEnvR, eEnvI))

  // 6) smart constructor and deconstructor
  def mkWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](env: Rep[EnvR], f: RFunc[EnvI, Iter[Row]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]): Rep[WrapStructRelation[Row, EnvR, EnvI]]
  def unmkWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](p: Rep[Relation[Row]]): Option[(Rep[EnvR], Rep[EnvI => Iter[Row]])]

  registerModule(Relations_Module)
}

// Std -----------------------------------
trait RelationsStd extends ScalanStd with RelationsDsl {
  self: RelationsDsl with ScalanSqlStd =>

  lazy val Relation: Rep[RelationCompanionAbs] = new RelationCompanionAbs {
  }

  lazy val PhysicalRelation: Rep[PhysicalRelationCompanionAbs] = new PhysicalRelationCompanionAbs {
  }

  case class StdIterBasedRelation[Row]
      (override val iter: RIter[Row])(implicit eRow: Elem[Row])
    extends AbsIterBasedRelation[Row](iter) {
  }

  def mkIterBasedRelation[Row]
    (iter: RIter[Row])(implicit eRow: Elem[Row]): Rep[IterBasedRelation[Row]] =
    new StdIterBasedRelation[Row](iter)
  def unmkIterBasedRelation[Row](p: Rep[Relation[Row]]) = p match {
    case p: IterBasedRelation[Row] @unchecked =>
      Some((p.iter))
    case _ => None
  }

  case class StdTableRelation[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsTableRelation[Row](table, scanId) {
  }

  def mkTableRelation[Row]
    (table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableRelation[Row]] =
    new StdTableRelation[Row](table, scanId)
  def unmkTableRelation[Row](p: Rep[PhysicalRelation[Row]]) = p match {
    case p: TableRelation[Row] @unchecked =>
      Some((p.table, p.scanId))
    case _ => None
  }

  case class StdIndexRelation[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsIndexRelation[Row](table, index, scanId) {
  }

  def mkIndexRelation[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexRelation[Row]] =
    new StdIndexRelation[Row](table, index, scanId)
  def unmkIndexRelation[Row](p: Rep[PhysicalRelation[Row]]) = p match {
    case p: IndexRelation[Row] @unchecked =>
      Some((p.table, p.index, p.scanId))
    case _ => None
  }

  case class StdWrapRelation[Row, Row2]
      (override val env: RRelation[Row2], override val f: RFunc[Iter[Row2], Iter[Row]])(implicit eRow: Elem[Row], eRow2: Elem[Row2])
    extends AbsWrapRelation[Row, Row2](env, f) {
  }

  def mkWrapRelation[Row, Row2]
    (env: RRelation[Row2], f: RFunc[Iter[Row2], Iter[Row]])(implicit eRow: Elem[Row], eRow2: Elem[Row2]): Rep[WrapRelation[Row, Row2]] =
    new StdWrapRelation[Row, Row2](env, f)
  def unmkWrapRelation[Row, Row2](p: Rep[Relation[Row]]) = p match {
    case p: WrapRelation[Row, Row2] @unchecked =>
      Some((p.env, p.f))
    case _ => None
  }

  case class StdWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct]
      (override val env: Rep[EnvR], override val f: RFunc[EnvI, Iter[Row]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI])
    extends AbsWrapStructRelation[Row, EnvR, EnvI](env, f) {
  }

  def mkWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct]
    (env: Rep[EnvR], f: RFunc[EnvI, Iter[Row]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]): Rep[WrapStructRelation[Row, EnvR, EnvI]] =
    new StdWrapStructRelation[Row, EnvR, EnvI](env, f)
  def unmkWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](p: Rep[Relation[Row]]) = p match {
    case p: WrapStructRelation[Row, EnvR, EnvI] @unchecked =>
      Some((p.env, p.f))
    case _ => None
  }
}

// Exp -----------------------------------
trait RelationsExp extends ScalanExp with RelationsDsl {
  self: RelationsDsl with ScalanSqlExp =>

  lazy val Relation: Rep[RelationCompanionAbs] = new RelationCompanionAbs {
  }

  lazy val PhysicalRelation: Rep[PhysicalRelationCompanionAbs] = new PhysicalRelationCompanionAbs {
  }

  object PhysicalRelationMethods {
  }

  case class ExpIterBasedRelation[Row]
      (override val iter: RIter[Row])(implicit eRow: Elem[Row])
    extends AbsIterBasedRelation[Row](iter)

  object IterBasedRelationMethods {
    object map {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[Row => B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "filter" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[IterBasedRelation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[IterBasedRelation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IterBasedRelation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "reduce" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "mapReduce" =>
          Some((receiver, mapKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sort {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "sort" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "sortBy" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object join {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key]) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "join" =>
          Some((receiver, other, thisKey, otherKey)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key]) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key]) forSome {type Row; type B; type Key}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object onlyValue {
      def unapply(d: Def[_]): Option[Rep[IterBasedRelation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "onlyValue" =>
          Some(receiver).asInstanceOf[Option[Rep[IterBasedRelation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IterBasedRelation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIterBasedRelation[Row]
    (iter: RIter[Row])(implicit eRow: Elem[Row]): Rep[IterBasedRelation[Row]] =
    new ExpIterBasedRelation[Row](iter)
  def unmkIterBasedRelation[Row](p: Rep[Relation[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IterBasedRelationElem[Row] @unchecked =>
      Some((p.asRep[IterBasedRelation[Row]].iter))
    case _ =>
      None
  }

  case class ExpTableRelation[Row]
      (override val table: Rep[Table], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsTableRelation[Row](table, scanId)

  object TableRelationMethods {
    object iter {
      def unapply(d: Def[_]): Option[Rep[TableRelation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TableRelationElem[_]] && method.getName == "iter" =>
          Some(receiver).asInstanceOf[Option[Rep[TableRelation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[TableRelation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkTableRelation[Row]
    (table: Rep[Table], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[TableRelation[Row]] =
    new ExpTableRelation[Row](table, scanId)
  def unmkTableRelation[Row](p: Rep[PhysicalRelation[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TableRelationElem[Row] @unchecked =>
      Some((p.asRep[TableRelation[Row]].table, p.asRep[TableRelation[Row]].scanId))
    case _ =>
      None
  }

  case class ExpIndexRelation[Row]
      (override val table: Rep[Table], override val index: Rep[Index], override val scanId: Rep[Int])(implicit eRow: Elem[Row])
    extends AbsIndexRelation[Row](table, index, scanId)

  object IndexRelationMethods {
    object iter {
      def unapply(d: Def[_]): Option[Rep[IndexRelation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IndexRelationElem[_]] && method.getName == "iter" =>
          Some(receiver).asInstanceOf[Option[Rep[IndexRelation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[IndexRelation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkIndexRelation[Row]
    (table: Rep[Table], index: Rep[Index], scanId: Rep[Int])(implicit eRow: Elem[Row]): Rep[IndexRelation[Row]] =
    new ExpIndexRelation[Row](table, index, scanId)
  def unmkIndexRelation[Row](p: Rep[PhysicalRelation[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexRelationElem[Row] @unchecked =>
      Some((p.asRep[IndexRelation[Row]].table, p.asRep[IndexRelation[Row]].index, p.asRep[IndexRelation[Row]].scanId))
    case _ =>
      None
  }

  case class ExpWrapRelation[Row, Row2]
      (override val env: RRelation[Row2], override val f: RFunc[Iter[Row2], Iter[Row]])(implicit eRow: Elem[Row], eRow2: Elem[Row2])
    extends AbsWrapRelation[Row, Row2](env, f)

  object WrapRelationMethods {
    object iter {
      def unapply(d: Def[_]): Option[Rep[WrapRelation[Row, Row2]] forSome {type Row; type Row2}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WrapRelationElem[_, _]] && method.getName == "iter" =>
          Some(receiver).asInstanceOf[Option[Rep[WrapRelation[Row, Row2]] forSome {type Row; type Row2}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WrapRelation[Row, Row2]] forSome {type Row; type Row2}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkWrapRelation[Row, Row2]
    (env: RRelation[Row2], f: RFunc[Iter[Row2], Iter[Row]])(implicit eRow: Elem[Row], eRow2: Elem[Row2]): Rep[WrapRelation[Row, Row2]] =
    new ExpWrapRelation[Row, Row2](env, f)
  def unmkWrapRelation[Row, Row2](p: Rep[Relation[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: WrapRelationElem[Row, Row2] @unchecked =>
      Some((p.asRep[WrapRelation[Row, Row2]].env, p.asRep[WrapRelation[Row, Row2]].f))
    case _ =>
      None
  }

  case class ExpWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct]
      (override val env: Rep[EnvR], override val f: RFunc[EnvI, Iter[Row]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI])
    extends AbsWrapStructRelation[Row, EnvR, EnvI](env, f)

  object WrapStructRelationMethods {
    object iter {
      def unapply(d: Def[_]): Option[Rep[WrapStructRelation[Row, EnvR, EnvI]] forSome {type Row; type EnvR <: Struct; type EnvI <: Struct}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[WrapStructRelationElem[_, _, _]] && method.getName == "iter" =>
          Some(receiver).asInstanceOf[Option[Rep[WrapStructRelation[Row, EnvR, EnvI]] forSome {type Row; type EnvR <: Struct; type EnvI <: Struct}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[WrapStructRelation[Row, EnvR, EnvI]] forSome {type Row; type EnvR <: Struct; type EnvI <: Struct}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct]
    (env: Rep[EnvR], f: RFunc[EnvI, Iter[Row]])(implicit eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]): Rep[WrapStructRelation[Row, EnvR, EnvI]] =
    new ExpWrapStructRelation[Row, EnvR, EnvI](env, f)
  def unmkWrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](p: Rep[Relation[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: WrapStructRelationElem[Row, EnvR, EnvI] @unchecked =>
      Some((p.asRep[WrapStructRelation[Row, EnvR, EnvI]].env, p.asRep[WrapStructRelation[Row, EnvR, EnvI]].f))
    case _ =>
      None
  }

  object RelationMethods {
    object iter {
      def unapply(d: Def[_]): Option[Rep[Relation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "iter" =>
          Some(receiver).asInstanceOf[Option[Rep[Relation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Relation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object map {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[Row => B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[Row => B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[Row => B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[Row => Relation[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "flatMap" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[Row => Relation[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[Row => Relation[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "filter" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[Relation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[Relation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Relation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "mapReduce" =>
          Some((receiver, mapKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sort {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "sort" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "sortBy" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object join {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key]) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "join" =>
          Some((receiver, other, thisKey, otherKey)).asInstanceOf[Option[(Rep[Relation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key]) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key]) forSome {type Row; type B; type Key}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object onlyValue {
      def unapply(d: Def[_]): Option[Rep[Relation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "onlyValue" =>
          Some(receiver).asInstanceOf[Option[Rep[Relation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Relation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Relations_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAN1YX2gcRRifvdzlcrmzptGWKhZjvFrxz11IkQpBStpc9Mo1OW5TI7Eoc7tzydbZ2c3u3LnnQwUf+qA+FfFB8KGg+FIE8c1CEVQQEUFfffapRkofLAoVZ2b/3Z/duyRtA3oPw+7Ot7/v+37fb2bnuytbIGVb4AlbgRiSgo4oLMjiet6meblEqEbbZwy1idECavz89NfG+DvvLyXAxBoY3YD2go3XQMa9KDlmcC1TtQIykCjIpoZlU/BYRXgoKgbGSKGaQYqarjcprGNUrGg2nauAZN1Q25vgApAqYEIxiGIhiuRTGNo2sr3nY4hHpAX3GXHfXjZDH6TIsyh2ZLFiQY2y8JmPCde+hky5TQzS1inY54W2bPKwmE0WOSbLoaybWLgZqYC0ppuGRX2vaeZhw1D92ySB7AGYrJyHLVhkXteLMrU0ss7BTKi8AdfREjPh5kmWg41wY6VtIg88a1O1y59jAgBYVWZFYIWQs0LAWYFzlpeRpUGsvQX5ZNUynDZwf9IIAI7JIJ4ZAuEjoBJR8++eU169JWf1BH/Z4aGkRUCjDOjRGIWI8jBuv69dsm++ePl4AoyvgXHNnq/b1IIK7ZSBR1cWEmJQEXPAILTWWQWn4yoovMwzmx6ZZBRDNyFhSB6XOVYorCka5cb8Wc4rTwz3aWoi31RyTCnIdyomX6GlUxDj6vWHnj3ye+mVBEh0u8gwSJktBssHpWCshnCY76gY76dgpGa8KVjmQ8YJx/SAAAIqjl7/Q/1uBpxLBAR6/rZXMwYx+fxHXx1B1c8TYGxNSHwRw3VRPc7QArKVNTBmtJDlPk+3IOZXkRVMq6gBm5h6vHYSMsIIoWAqdnWaiLM1J1Qv+elnXeEuGQTlF6v5P+UfPrjCdWmBnDvjLtd/tOO3f93XoEKyFCQ1iiyf3lSt7N9tg3I+HBZmBzpeOST5MYl55gExBB8tWcJI34YDCvbzUE5CG6m+FEJ/vFSH4+Qm5HmwVnkQb524lgCp0yDVYDWwKyBVN5pE9XXP9kuKHHrSfyZ114DpHFpQ9yXi7hJTQAQhwu2LXJhmpe7sJqobbVtj4e1Y0H3Egh5iU2JTCpGQ2Y2dWuHzvQH2wYwyIklZjcCxwOPxJFctTWfflRZ67purZ29cW0qJZT3pifpliJvI3dI9PkNuueykGeapTOgei+o+QUmUoPhY/P8WOaURFTkDYMp8fvda4cNLe11MEfNdL+bOPz1DiziCSMvHydSi8ZMMf3ZoAaRGuFcvNonSg9G/fUfCRpvGZnhPqxkNkuIgswNQevPyRZFbtaC5O03Em0SSuMe66V+zyRJp1YYohg+XBkqGgZTDcP4LyuBpD1JGJC2RMOUhMOUIgU1ygbEvW1Ohd0Nm7CM77h7PZENH+6dvaq9dfo+KE6rkdHdIy/XzrCOZE5E84oc56oYSBJruJKDHFb/bGvJif8rBYSDucOw3i19cvHjgxievPyAamrG6RnVo5md20M7w69y9bVdAj+JY2He2irvWckimqHxnH7MzjXQw/xe4swBjQ+s7tOwmxHD8O3x9i4mlECOWBaRgaCGV9+5IR8STwbEPT6yePrR6Vgg/pwojdybog6L/CTkDzTnRtz85oG9nRvmSbtI2vzj27Qu/vP3jZ5+KBqhT/pmAWeqtSVKwN3GQ0nRMSrKnOKalC7c+Xnrqpy9/E2fhca5d1o2R4F+QzjNwd/lyge8FG3dtxxnXk7yJO2hna5XrPFzR0kE+PPwvK61f750SAAA="
}
}

