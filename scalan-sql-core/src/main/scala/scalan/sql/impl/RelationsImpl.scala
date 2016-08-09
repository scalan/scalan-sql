package scalan.sql

import java.lang.reflect.Method
import scalan._
import scalan.common.Lazy
import scala.reflect.runtime.universe._
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
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+MzuzMz0zxE3iSgSD6zJRUTMTjBBhkbDZnZUsnd1lOhIZg1LTXTPpWN1d21W79niI4CEH9STiQfAQULwEQbx5EA8KEkRIrp49xYjkYE6Kr6p/Ziakk83BORTVVa/ez/d9r2qu3oKyCOFpYRNG/JZHJWlZer4sZNPq+NKVozOBs8PoKh1cf/6HoPb+RxtFmOvB7AUiVgXrgRFPOhHP5pZ0TDCIb1Mhg1BIeMrUEdp2wBi1pRv4bdfzdiTpM9o2XSGXTCj1A2e0DZegYMKcHfh2SCW1VhgRgopkvUpVRm72bejv0SYfx/Dbqor2RBVnQ+JKTB9jzMX2XcqtkR/4I0/CviS1Ta7SQps6jTjWcNrjTIeZMaHiejwIZRq1ghEuBE76WfIJLsAB8yLZJW2MOmxbMnT9oXLGif02GdINNFHmJaxBUDY4O+I0cV4X0pmKF3EAQFZe1Im1xpi1MsxaCrOmRUOXMPddoja3wiAaQfwrzABEHF288AAXqQfa8Z3mB+ftN+5Yda+oDkcqlYpOaBYdPZmjEE0PYvtz92Nx+9UrJ4pQ60HNFct9IUNiy0kZJHDVie8HUuecIUjCITK4mMegjrKMNnfJxLADjxMfPSVYNpAo5tquVMZqrZHQk4N9RXKamhYiXsjqXcipV2tphTC2dfPxo0f+6LxehOJ0CANdWtgMYepUQrVL2bjeWT0+ImGmG7yjUVaDEY3Hyn0SyKB45uafzk/H4HwxAzCJtzfO0MWBlz/77gjd+roI1Z6W+BojQ82eQmiVCrsH1WCXhvF6ZZcwNbsngxWHDsgOkwmuk4DMICASFnK7k1OF1pJWfSEtvx4LdyPwaXNtq/m3de2Tq0qXITTinbhd/3VP/PPbvoHUkpVQciUNU3jL3dPp1x4gV8NhbTY/ceRQIc1J72MEih5Sb6UOo94eAkjYr1I5RQR1UimM4ymqDufJTcvzsa75KLt18vsilNehPEAOhAnlfrDjO6nu8b6UNJKn0rXCNAeocxISL5VIfEssgE5Cp5uT+QO1lN6t31y+PP/XF28d1P1f7bvSI7x57CG6P23W/7G74S6qMO3plYftU2PikpzPLGKpTLb9JNdqbO8ZfzW+FB8PoRYr3wo8un/xtvvmlQ+lbv5CNP34bPYv4mW/pA8/gedaOdytUpuRkDrq5aEevoxx7cc/PXlu/dC517TvhqON4p2si+/9jp8hfEm/Os/e59VBo2bH4/ivAifHf3zlxnu/fPWlbt8xgBKMjAmZlO23xHb8Pq5ghMWckqxEAEjtpTufbzz367e/6wu6pqSEd4mfveFj3WQ3ZkJ3I4uNr/KYYNwx4kjWNpsgCq8BJbskNTWuqWH9PyjOd2NbCQAA"
}
}

