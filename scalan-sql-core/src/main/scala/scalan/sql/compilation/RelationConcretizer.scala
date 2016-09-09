package scalan.sql.compilation

import scalan.sql.ScalanSqlExp

// could also extend bridge instead of having it as a parameter
class RelationConcretizer[+S <: ScalanSqlExp](val bridge: ScalanSqlBridge[S]) {
  import bridge.scalan._

  private def allFieldCombinations(fields: List[(String, List[Rep[_]])]): List[List[(String, Rep[_])]] = fields match {
    case Nil =>
      Nil
    case (name, fields) :: Nil =>
      List(fields.map(name -> _))
    case (name, fields) :: tail =>
      val tailCombinations = allFieldCombinations(tail)
      for {
        tailFields <- tailCombinations
        field <- fields
      } yield
        (name, field) :: tailFields
  }

  def concretePlans[A](f: RFunc[Struct, Relation[A]]) = f // match {
//    case Def(l: Lambda[in, _]) =>
//      val inputVar = l.x.asRep[Struct]
//      val allCandidateFields: List[(String, List[Rep[_]])] = inputVar.fields.map {
//        case (name, field) =>
//          field.elem match {
//            case elem: ScannableElem[r, _] =>
//
//              val eRow = elem.eRow
//              val ((table, candidateIndices), scanId) = field.getMetadata(CandidateIndicesKey).get
//              val candidateScannables =
//                TableScannable(table, scanId)(eRow) +: candidateIndices.map(index => IndexScannable(table, index, scanId)(eRow))
//              (name, candidateScannables)
//          }
//      }.toList
//
//      val candidateFieldCombinations = allFieldCombinations(allCandidateFields)
//
//      candidateFieldCombinations.map { fields =>
//        val inputStruct = struct(fields)
//        f(inputStruct)
//      }
//  }
}
