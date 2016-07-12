package scalan.sql

import java.lang.reflect.Method
import scalan._
import scalan.common.Lazy
import scala.reflect.runtime.universe._
import scalan.common.Default

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

  registerModule(Iters_Module)
}

// Std -----------------------------------
trait ItersStd extends ScalanStd with ItersDsl {
  self: ItersDsl with ScalanSqlStd =>
  lazy val Iter: Rep[IterCompanionAbs] = new IterCompanionAbs {
  }
}

// Exp -----------------------------------
trait ItersExp extends ScalanExp with ItersDsl {
  self: ItersDsl with ScalanSqlExp =>
  lazy val Iter: Rep[IterCompanionAbs] = new IterCompanionAbs {
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
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "mapU" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
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
  val dump = "H4sIAAAAAAAAAIVUPWwURxR+d/b5fD4bHAIIoiBs6/JLuENQUFiRZexDsrX+kdfIkYMSze2OzwuzM+PdMeylMFIKFyElokuBBKJxg+gRBWkihJS0qakMKEoBFVHezO7t3Tm6yxaj3dk3733v+743e68hFwbwaegQRnjZp4qUbfM+HaqSXeXKU40F4W4zOks3fj/zVAz9+PNiFkbXYWCThLMhW4dC/FKNZPpuK9eCAuEODZUIQgXjlqlQcQRj1FGe4BXP97cVqTFasbxQTVrQXxNuYwt2IGPBqCO4E1BF7RlGwpCGyf4g1Yi89LtgvhtLslWDV3QXlbYuVgPiKYSPNUbj+BUq7QYXvOErOJRAW5IaFsYUaSSxhzlfMlOmz4K850sRqGbVPFbYFG7zs58T3IAj1jVyg1Swar1iq8DjdZ1MEuc6qdNFDNHh/dhDSNnGakPSJHkxVG5HvUgCAKpy3gArtzgrp5yVNWclmwYeYd4PRP9cDkTUgPjJ9AFEElN89T8pmhlolbuln646376zi35WH440lLxZDKoBzHa6i02aBD/a3T321/3vP8zC0DoM1jzlE1k61+6EhLEi4VwoAzslkQR1FHGim4jGB9MYc8ApBUf4knDMlNA5jFoxz/GUDtZ7w4lCXejPK0mboRmkPu12rFe3M4Sx5f2TZz95Vf0mC9nOEgVMaeM8BM2kCvqQniSzXg8r6J9TNOjc6lsRN1PaC9HBNd8DVErPZ/tv3F/PwdVsSmqCoU3PAIZiX9jCpx9M/O19d++2MgcyUaePl2rX0DeTUQDD8Yl4Sv7xLr7/89CGMk4xBYpwwC8nOuoV49OLgtPS5eXSW/u3O3v6sP7/MdIzosmYaVdy3ByVCU9mJ1XmVDdljJLHV6yj7PXUkyzk5iG3wUg9tCBXE9vcbVoEbxdFI3WpuZfp1A8tQQLip5Ywzxi0YPWQaqQF9SOEWu4CdZY6jATU1VNPfbyV4mG4cHdqbf7E2hUjxrBrguI/CsZ63aELRE6aif+8x8RjUKnqS7zR8eXCs6//uPX84QOjYUssBTljTJV4hJfDLZa2M9GlHTuZKXT5zrtfFr988filmYohPZ0oOlfhf3hOnJO6f9DUxZuwZSLcLcRV7C3WRje6Qk9xAkuvX+jl7L/s79MizwYAAA=="
}
}

