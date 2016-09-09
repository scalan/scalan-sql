package scalan.sql

import scalan._
import scala.reflect.runtime.universe.{WeakTypeTag, weakTypeTag}
import scalan.meta.ScalanAst._

package impl {
// Abs -----------------------------------
trait KernelInputsAbs extends Scalan with KernelInputs {
  self: KernelInputsDsl with ScalanSql =>

  // single proxy for each type family
  implicit def proxyKernelInput(p: Rep[KernelInput]): KernelInput = {
    proxyOps[KernelInput](p)(scala.reflect.classTag[KernelInput])
  }

  // familyElem
  class KernelInputElem[To <: KernelInput]
    extends EntityElem[To] {
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs()
    override def isEntityType = true
    override lazy val tag = {
      weakTypeTag[KernelInput].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[KernelInput] => convertKernelInput(x) }
      tryConvert(element[KernelInput], this, x, conv)
    }

    def convertKernelInput(x: Rep[KernelInput]): Rep[To] = {
      x.selfType1 match {
        case _: KernelInputElem[_] => x.asRep[To]
        case e => !!!(s"Expected $x to have KernelInputElem[_], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def kernelInputElement: Elem[KernelInput] =
    cachedElem[KernelInputElem[KernelInput]]()

  implicit case object KernelInputCompanionElem extends CompanionElem[KernelInputCompanionAbs] {
    lazy val tag = weakTypeTag[KernelInputCompanionAbs]
    protected def getDefaultRep = KernelInput
  }

  abstract class KernelInputCompanionAbs extends CompanionDef[KernelInputCompanionAbs] {
    def selfType = KernelInputCompanionElem
    override def toString = "KernelInput"
  }
  def KernelInput: Rep[KernelInputCompanionAbs]
  implicit def proxyKernelInputCompanionAbs(p: Rep[KernelInputCompanionAbs]): KernelInputCompanionAbs =
    proxyOps[KernelInputCompanionAbs](p)

  registerModule(KernelInputs_Module)
}

// Std -----------------------------------
trait KernelInputsStd extends ScalanStd with KernelInputsDsl {
  self: KernelInputsDsl with ScalanSqlStd =>

  lazy val KernelInput: Rep[KernelInputCompanionAbs] = new KernelInputCompanionAbs {
  }
}

// Exp -----------------------------------
trait KernelInputsExp extends ScalanExp with KernelInputsDsl {
  self: KernelInputsDsl with ScalanSqlExp =>

  lazy val KernelInput: Rep[KernelInputCompanionAbs] = new KernelInputCompanionAbs {
  }

  object KernelInputMethods {
  }
}

object KernelInputs_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAAIVUz2sTQRSepNE0P2yrIrQIWkP8gT+SYg8eikhpI1TTtLiVSi3KZHeaTp2dne5OZeOhgoce1KN48yAoXnoR/wEPehER9OrZU1XEgz0pvpndbJJK4h6G3Zm37/ve9743m9/QLs9FxzwTM8wLNpG4YOj3cU/mjRKXVNanHWuNkUmy9PHUayd972EljgYW0O5l7E16bAGlgpeSL6J3Q1pllMLcJJ50XE+iI2WNUDQdxogpqcOL1LbXJK4yUixTT46VUaLqWPVVtI5iZTRgOtx0iSTGBMOeR7xwv5coRjT6Tunv+oxoYvCiqqLYUsWci6kE+oAxEMRfIcKoc4fXbYn6QmozQtGCmAzxBdQwZQumYXrKKElt4biygZoEhGXHanwmOIYNtK+8gm/jIqDWioZ0Ka+pZAKbt3CNVCBEhSegBo+wpbm6IGHyjCetNjxfIISgK2c1sUJTs0KkWUFpljeISzGjd7A6nHUdv46CJ9aDkC8gxen/pGhkICVu5e8vmte3jYwdVz/7ikpSL5rVbsh2uINNGgK/3Ng48OPZzf1xlF5AvVUqbSzyI61OCBXLYM4dqWlHImK3Bk3MdWqi9sE4xOxwSsp0bIE5ZArlzEKvGDWpVMFqLxt2qIP8SSlIIzQG0kfVDnerdgIzNrs1dObo19K1OIq3Q6QgpQHz4DaSStQD8oSZ1dovUeYycTlhU1ysyUjrlN9ck104RGoc3/puvR1Bi/FIwxCypX0uWEzboOJwkr84m/9lvHu0qbrsomxwEpj/Dz33+3PfktQGEP9SHNrBc4/ehPzpIIvh2GRv7ie98fSB1IRifvtYzFRXwIZj+udB+K/QobxJYjLsEkvNBLFhZgOrjD6+MH9pcP6qzp21dFBwItFwtxtmGosxPQ8nuswDBOVLtoD7Dl5G35z/dPf9i+daiqaWEmVbNAHYdFiBtxoM70EAyXWoygiNB1ZY335SOfnh1RdtnbSyMLSGy7YbRZsJWLe5pr8VHm6N5oDCYSoAM1ZZS6MkSijHh+zUekgtub9C7yBm+wUAAA=="
}
}

trait KernelInputsDsl extends impl.KernelInputsAbs {self: KernelInputsDsl with ScalanSql =>}
trait KernelInputsDslStd extends impl.KernelInputsStd {self: KernelInputsDsl with ScalanSqlStd =>}
trait KernelInputsDslExp extends impl.KernelInputsExp {self: KernelInputsDsl with ScalanSqlExp =>}
