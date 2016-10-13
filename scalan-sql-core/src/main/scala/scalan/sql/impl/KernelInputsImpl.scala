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
  val dump = "H4sIAAAAAAAAAIVUz2sTQRSepLFpfthWRWwRtIb4A9Gk2IOHIlLaiNX0B26l0pbKZHeaTp2dne5OZeOh3grqVbx5UBQvvYj/gl5EPHj17Kkq4kFBUHwzu9kklcQ9DLszb9/3ve99b7a/oD2ei054JmaYF2wiccHQ72OezBslLqmsTTnWBiMTZGX58tNfi/a9Q3HUv4C6V7E34bEFlApeSr6I3g1plVEKc5N40nE9iY6VNULRdBgjpqQOL1Lb3pC4wkixTD05WkaJimPV1tEmipVRv+lw0yWSGOMMex7xwv0eohjR6Dulv2szooHBi6qKYlMVcy6mEugDRn8Qf40Io8YdXrMl6g2pzQhFC2IyxBdQw6QtmIbpKqMktYXjyjpqEhBWHav+meAYNtD+8hq+jYuAWi0a0qW8qpIJbN7CVTINISo8ATV4hK3M1QQJk2c8abXg+QIhBF05p4kVGpoVIs0KSrO8QVyKGb2D1eGs6/g1FDyxLoR8ASnO/CdFPQMpcSt/f8lc/Glk7Lj62VdUknrRrLoh29E2NqkL/HJr6+C3ZzcPxFF6AfVUqLSxyA83OyFULIM5d6SmHYmI3So0MdeuidoHYxCzyykp07EF5pAplDMLvWLUpFIFq71s2KE28ielIPXQGEgfVTvUqdpxzNjszuDZ459LN+Io3gqRgpQGzINbTypRF8gTZlZrn0SZq8TlhE1ysSEjrVN+Y0124BCpcXLnq/VmGC3FIw1DyKb2uWAxbYNph5P8pdn8D+Ptw23VZRdlg5PA/H/o+d8fe1ekNoD4l+LgLp579SbkTwdZDMcm+3Lf6fKTB1ITivmtYzFTWQMbjuqfB+C/QpvyJojJsEssNRPEhpkNrDLy6OL8lYH56zp31tJBwYlEQ51umCksRvU8nOowDxCUL9kC7jt4GXl94cPddy+eaykaWkqUbdIEYNNhBd56MLyHASTXpiojNB5YYfPn4+nT71990tZJKwtDa7hsuVG0mYB1i2v6muHh1mgMKBymAjBjnTU1SqKEcnzITq1H1JL7C+vYlx37BQAA"
}
}

trait KernelInputsDsl extends impl.KernelInputsAbs {self: KernelInputsDsl with ScalanSql =>}
trait KernelInputsDslStd extends impl.KernelInputsStd {self: KernelInputsDsl with ScalanSqlStd =>}
trait KernelInputsDslExp extends impl.KernelInputsExp {self: KernelInputsDsl with ScalanSqlExp =>}
