package scalan.sql

import scalan.sql.parser.SqlAST._

case class Constraint(lhs: ResolvedTableAttribute, op: ComparisonOp, rhs: Expression)

class ConstraintSet private (val asMap: Map[ResolvedTableAttribute, Map[ComparisonOp, Set[Expression]]]) {
  def this() = this(Map.empty)

  def addConstraints(predicate: Expression) = {
    val clauses = conjunctiveClauses(predicate)
    val extractedConstraints = clauses.flatMap {
      case clause @ BinOpExpr(op: ComparisonOp, l, r) =>
        (underlyingTableColumn(l), underlyingTableColumn(r)) match {
          case (None, None) =>
            Nil
          case (Some(l1), None) =>
            List(Constraint(l1, op, r))
          case (None, Some(r1)) =>
            List(Constraint(r1, op.inverse, l))
          case (Some(l1), Some(r1)) =>
            List(
              Constraint(l1, op, r1),
              Constraint(r1, op.inverse, l1)
            )
        }
      case _ =>
        Nil
    }

    val newConstraints = extractedConstraints.foldLeft(asMap) {
      case (mapAcc, Constraint(attr, op, rhs)) =>
        val currentConstraintsForAttr = mapAcc.getOrElse(attr, Map.empty)
        val currentConstraintsForAttrAndOp = currentConstraintsForAttr.getOrElse(op, Set.empty)
        if (currentConstraintsForAttrAndOp.contains(rhs))
          mapAcc
        else
          mapAcc.updated(attr, currentConstraintsForAttr.updated(op, currentConstraintsForAttrAndOp + rhs))
    }

    new ConstraintSet(newConstraints)
  }
}

object ConstraintSet {
  val empty = new ConstraintSet
}
