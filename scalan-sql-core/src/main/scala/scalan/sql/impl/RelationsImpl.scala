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

  abstract class AbsPhysicalRelation[Row]
      (source: RScannable[Row])(implicit eRow: Elem[Row])
    extends PhysicalRelation[Row](source) with Def[PhysicalRelation[Row]] {
    lazy val selfType = element[PhysicalRelation[Row]]
  }
  // elem for concrete class
  class PhysicalRelationElem[Row](val iso: Iso[PhysicalRelationData[Row], PhysicalRelation[Row]])(implicit override val eRow: Elem[Row])
    extends RelationElem[Row, PhysicalRelation[Row]]
    with ConcreteElem[PhysicalRelationData[Row], PhysicalRelation[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(relationElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertRelation(x: Rep[Relation[Row]]) = // Converter is not generated by meta
!!!("Cannot convert from Relation to PhysicalRelation: missing fields List(source)")
    override def getDefaultRep = PhysicalRelation(element[Scannable[Row]].defaultRepValue)
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[PhysicalRelation[Row]]
    }
  }

  // state representation type
  type PhysicalRelationData[Row] = Scannable[Row]

  // 3) Iso for concrete class
  class PhysicalRelationIso[Row](implicit eRow: Elem[Row])
    extends EntityIso[PhysicalRelationData[Row], PhysicalRelation[Row]] with Def[PhysicalRelationIso[Row]] {
    override def from(p: Rep[PhysicalRelation[Row]]) =
      p.source
    override def to(p: Rep[Scannable[Row]]) = {
      val source = p
      PhysicalRelation(source)
    }
    lazy val eFrom = element[Scannable[Row]]
    lazy val eTo = new PhysicalRelationElem[Row](self)
    lazy val selfType = new PhysicalRelationIsoElem[Row](eRow)
    def productArity = 1
    def productElement(n: Int) = eRow
  }
  case class PhysicalRelationIsoElem[Row](eRow: Elem[Row]) extends Elem[PhysicalRelationIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new PhysicalRelationIso[Row]()(eRow))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[PhysicalRelationIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class PhysicalRelationCompanionAbs extends CompanionDef[PhysicalRelationCompanionAbs] {
    def selfType = PhysicalRelationCompanionElem
    override def toString = "PhysicalRelation"

    @scalan.OverloadId("fromFields")
    def apply[Row](source: RScannable[Row])(implicit eRow: Elem[Row]): Rep[PhysicalRelation[Row]] =
      mkPhysicalRelation(source)

    def unapply[Row](p: Rep[Relation[Row]]) = unmkPhysicalRelation(p)
  }
  lazy val PhysicalRelationRep: Rep[PhysicalRelationCompanionAbs] = new PhysicalRelationCompanionAbs
  lazy val PhysicalRelation: PhysicalRelationCompanionAbs = proxyPhysicalRelationCompanion(PhysicalRelationRep)
  implicit def proxyPhysicalRelationCompanion(p: Rep[PhysicalRelationCompanionAbs]): PhysicalRelationCompanionAbs = {
    proxyOps[PhysicalRelationCompanionAbs](p)
  }

  implicit case object PhysicalRelationCompanionElem extends CompanionElem[PhysicalRelationCompanionAbs] {
    lazy val tag = weakTypeTag[PhysicalRelationCompanionAbs]
    protected def getDefaultRep = PhysicalRelation
  }

  implicit def proxyPhysicalRelation[Row](p: Rep[PhysicalRelation[Row]]): PhysicalRelation[Row] =
    proxyOps[PhysicalRelation[Row]](p)

  implicit class ExtendedPhysicalRelation[Row](p: Rep[PhysicalRelation[Row]])(implicit eRow: Elem[Row]) {
    def toData: Rep[PhysicalRelationData[Row]] = isoPhysicalRelation(eRow).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoPhysicalRelation[Row](implicit eRow: Elem[Row]): Iso[PhysicalRelationData[Row], PhysicalRelation[Row]] =
    reifyObject(new PhysicalRelationIso[Row]()(eRow))

  // 6) smart constructor and deconstructor
  def mkPhysicalRelation[Row](source: RScannable[Row])(implicit eRow: Elem[Row]): Rep[PhysicalRelation[Row]]
  def unmkPhysicalRelation[Row](p: Rep[Relation[Row]]): Option[(Rep[Scannable[Row]])]

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

  case class StdPhysicalRelation[Row]
      (override val source: RScannable[Row])(implicit eRow: Elem[Row])
    extends AbsPhysicalRelation[Row](source) {
  }

  def mkPhysicalRelation[Row]
    (source: RScannable[Row])(implicit eRow: Elem[Row]): Rep[PhysicalRelation[Row]] =
    new StdPhysicalRelation[Row](source)
  def unmkPhysicalRelation[Row](p: Rep[Relation[Row]]) = p match {
    case p: PhysicalRelation[Row] @unchecked =>
      Some((p.source))
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

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => Relation[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "flatMap" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[Row => Relation[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[Row => Relation[B]]) forSome {type Row; type B}] = exp match {
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

    object partialMapReduce {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, mapKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "partialMapReduce" =>
          Some((receiver, prefixComparator, mapKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
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

    object partialSort {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, suffixComparator, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "partialSort" =>
          Some((receiver, prefixComparator, suffixComparator)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hashJoin {
      def unapply(d: Def[_]): Option[(Rep[IterBasedRelation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key], Boolean) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, leftIsOuter, _*), _) if receiver.elem.isInstanceOf[IterBasedRelationElem[_]] && method.getName == "hashJoin" =>
          Some((receiver, other, thisKey, otherKey, leftIsOuter)).asInstanceOf[Option[(Rep[IterBasedRelation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key], Boolean) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[IterBasedRelation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key], Boolean) forSome {type Row; type B; type Key}] = exp match {
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

  case class ExpPhysicalRelation[Row]
      (override val source: RScannable[Row])(implicit eRow: Elem[Row])
    extends AbsPhysicalRelation[Row](source)

  object PhysicalRelationMethods {
    object iter {
      def unapply(d: Def[_]): Option[Rep[PhysicalRelation[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[PhysicalRelationElem[_]] && method.getName == "iter" =>
          Some(receiver).asInstanceOf[Option[Rep[PhysicalRelation[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[PhysicalRelation[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  def mkPhysicalRelation[Row]
    (source: RScannable[Row])(implicit eRow: Elem[Row]): Rep[PhysicalRelation[Row]] =
    new ExpPhysicalRelation[Row](source)
  def unmkPhysicalRelation[Row](p: Rep[Relation[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PhysicalRelationElem[Row] @unchecked =>
      Some((p.asRep[PhysicalRelation[Row]].source))
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

    object partialMapReduce {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, mapKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "partialMapReduce" =>
          Some((receiver, prefixComparator, mapKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean], Rep[Row => K], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
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

    object partialSort {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(prefixComparator, suffixComparator, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "partialSort" =>
          Some((receiver, prefixComparator, suffixComparator)).asInstanceOf[Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], Rep[((Row, Row)) => Boolean], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object hashJoin {
      def unapply(d: Def[_]): Option[(Rep[Relation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key], Boolean) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, leftIsOuter, _*), _) if receiver.elem.isInstanceOf[RelationElem[_, _]] && method.getName == "hashJoin" =>
          Some((receiver, other, thisKey, otherKey, leftIsOuter)).asInstanceOf[Option[(Rep[Relation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key], Boolean) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Relation[Row]], RRelation[B], Rep[Row => Key], Rep[B => Key], Boolean) forSome {type Row; type B; type Key}] = exp match {
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
  val dump = "H4sIAAAAAAAAANVXTYgcRRSu+dv5JW5WQiIYsi4TFdGZsDlEWEQ22VndMLs7TEdWJmG1prtmtmN1dW9XzdjjId4C6k1EQfAQUbwERbyZq0IQ8eBJ0KunqEgOBgTFV9XTPT/bsz9qJM6hqOp+/b33vvdVzavrP6MUd9HDXMcUs5JFBC5par7IRVGrMGGK3qptdChZIq3NZ9///aL12tE4mm6gqS3MlzhtoKw/qXhOONeEUUVZzHTChe1ygR6qKg9l3aaU6MK0Wdm0rI7ATUrKVZOLhSpKNm2jt42uoFgVTes2010iiHaOYs4J7z/PEBmRGa6zat1bdwY+WFlmUR7K4oKLTQHhg49p375OHK3HbNazBDrUD23dkWGBTZ54DuSwYjlUuUlUUdq0HNsVgdc0eNiyjWCZZBgeoJnqZdzFZfDaLmvCNVlbgjlYfwm3yRqYSPMk5MAJbV3oOaQPnufCGPHnOQghqMq8Cqw04KwUclaSnBU14pqYmq9g+bLm2l4P+b9YAiHPAYjH94AIEEiFGcXXL+kX72h5Ky4/9mQoaRXQFACdmKAQVR7g9mb9TX77mWtn4ijXQDmTLza5cLEuhmXQpyuPGbOFijlkELttqODcpAoqL4tgMyaTrG5bDmaA1OeyAIWipm4KaSyfFfrlmcB9WjgkMI15TizMd3ZCvkpL5zCltVsPPHHyp8rzcRQfdZEFSA02gxuACpSpEzrId0qN9wmUqNsvK5blkPUGY3qXAEIqHrn1i/HlKXQpHhLY97e/mgHEzJPvfn6S1D6Oo0xDSXyZ4raqnmRoiXC9gTJ2l7j+83QXUzmLrGDaIC3coaLP6zAhCSBEoNmJu9Mhkq0FpfpYkH7eF+6azUhxuVb8TfvqretSly4q+G/87fqneeaP7w+1hJKsQElTEDegN1VfCVb7oFwOx5XZkaFPjsWCmNR78EAAIUBLViix9uFAoMMylLOYEyOQwsCfLNXxSXJT8mRHtdV3PjmxGUep8yjVghrwKko17Q4zAt3DeSmIJ84Gz2KjNQCdYxdbgUT8U2IWqSBUuDsiV6b52Gh2BxfyDkLRGKFT3O64OgmgcnXInjF5QN0blZuubfW4CRWJKpwcy/cQmQnCugFOth6NnwT8+fHYdiDFWoNttNxh+hjGzp0VCRttOjHDu1rQaJCUBJnfBWU8r0AXhQ0XO39PE5NNIkn8j3WTgOZojIIK69b3UIwcXtxVMgCyMgjn/6AMmfZuyoikJRJmZQ+YlQiBzUiBQR/Z0cW/ITP4k8n5/5yabZHDc7fNzWtvCNU8xLzR5nW9eRmaxQUVyYNBmFN+KGGg6WECxlzJ1Y09PtyZ8p59S9DHf3r16pFfP3jhftVrZpqmsLBTPHWATlPOC3e3k0RjioOw/9kuHtnLAzJV5YdbzINpZBz/5uDzG1CJ0oRKLBGdYpcY8s5CLLhT+ZmcfvvpjfPHNp5TqioYysh/E/Z/0TfAVewsqPvKo7vcV8CoWLEcuI/C5PQXT3376tcffagav2FtZUNeRV/wrMS3aZjS3ISUtH45oVBX7ry39tg3n/2oWvucFAZ0oSy8/Q1U4I0dlIXQN9znRs66rO9J26ZDtMNGkCIa2i7fyeGHvwAhg5bBlQ8AAA=="
}
}

