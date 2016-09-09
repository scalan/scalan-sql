package scalan.sql.compilation

import scalan.sql.ScalanSqlExp

class RelationToIterBridge[+S <: ScalanSqlExp](val scalan: S) {
  import scalan._

  // f takes a struct with (Int => Relation[...]) fields
  // returns a function from struct with (Int => Iter[...]) fields
  def relationFunToIterFun[A](f: Rep[Struct => Relation[A]]): Rep[Struct => Iter[A]] = {
    f.elem.eDom match {
      case se: StructElem[_] =>
        val iterInputElemFields = se.fields.map {
          case (name, FuncElem(_, re: RelationElem[r, _])) =>
            val eR = re.eRow
            (name, FuncElem(IntElement, iterElement(eR)))
        }
        val iterInputElem = structElement(iterInputElemFields)
        val resultFun = inferredFun(iterInputElem) { x =>
          val relationStructFields = se.fieldNames.map {
            case name =>
              x.getUntyped(name) match {
                case fieldF: RFunc[Int, Iter[a]] @unchecked =>
                  val fieldF1 = inferredFun(IntElement) { i =>
                    val iter = fieldF(i)
                    iterBasedRelation(iter)
                  }
                  (name, fieldF1)
              }
          }
          val relationStruct = struct(relationStructFields)
          val finalRelation = f(relationStruct)
          finalRelation.iter
        }
        for (query <- f.getMetadata(QueryTextKey)) {
          resultFun.setMetadata(QueryTextKey)(query)
        }
        resultFun
    }
  }
}
