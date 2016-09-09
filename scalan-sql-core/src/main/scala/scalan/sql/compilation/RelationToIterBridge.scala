package scalan.sql.compilation

import scalan.sql.ScalanSqlExp

class RelationToIterBridge[+S <: ScalanSqlExp](val scalan: S) {
  import scalan._

  // f takes a struct with (Int => Relation[...]) fields
  // returns a function from struct with (Int => Iter[...]) fields
  def relationFunToIterFun[A](f: Rep[KernelInput => Relation[A]]): Rep[KernelInput => Iter[A]] = f match {
    case Def(l: Lambda[KernelInput, Relation[A]]) =>
      inferredFun(kernelInputElement) { x =>
        // using simply f(x).iter rebuilds the plan
        val relation1 = mirrorApply(l, x)
        relation1.iter
      }
  }

}
