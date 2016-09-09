package scalan.sql.compilation

import scalan.sql.ScalanSqlExp

class RelationToIterBridge[+S <: ScalanSqlExp](val scalan: S) {
  import scalan._

  def relationFunToIterFun[A](f: Rep[KernelInput => Relation[A]]): Rep[KernelInput => Iter[A]] =
    inferredFun(kernelInputElement) { x => f(x).iter }.
      setMetadata(QueryTextKey)(f.getMetadata(QueryTextKey).getOrElse {
        !!!("No query text metadata")
      })
}
