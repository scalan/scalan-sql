package scalan.sql

import scalan.sql.parser.SqlAST._

case class Constraint(lhs: ResolvedTableAttribute, op: ComparisonOp, rhs: Expression)

class ConstraintSet private (val asMap: Map[ResolvedTableAttribute, Map[ComparisonOp, Set[Expression]]]) {
  // only call when && are eliminated
  private def extractConstraints(clause: Expression) = clause match {
    case BinOpExpr(op: ComparisonOp, l, r) =>
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

  def addConstraints(predicate: Expression) = {
    val clauses = conjunctiveClauses(predicate)
    val extractedConstraints = clauses.flatMap(extractConstraints)

    val newConstraints = extractedConstraints.foldLeft(asMap) {
      case (mapAcc, Constraint(attr, op, rhs)) =>
        val currentConstraintsForAttr = mapAcc(attr)
        val currentConstraintsForAttrAndOp = currentConstraintsForAttr(op)
        if (currentConstraintsForAttrAndOp.contains(rhs))
          mapAcc
        else
          mapAcc.updated(attr, currentConstraintsForAttr.updated(op, currentConstraintsForAttrAndOp + rhs))
    }

    new ConstraintSet(newConstraints)
  }

  def union(other: ConstraintSet): ConstraintSet = {
    val unionConstraints = other.asMap.foldLeft(asMap) {
      case (acc, (lhs, otherConstraintsForLhs)) =>
        val allConstraintsForLhs = otherConstraintsForLhs.foldLeft(acc(lhs)) {
          case (acc2, (op, otherRhses)) =>
            acc2.updated(op, acc2(op).union(otherRhses))
        }
        acc.updated(lhs, allConstraintsForLhs)
    }
    new ConstraintSet(unionConstraints)
  }

  def simplify(predicate: Expression): Option[Expression] = {
    val clauses = conjunctiveClauses(predicate)
    val simplifiedClauses = clauses.flatMap(simplifyClause).distinct
    simplifiedClauses.reduceOption(BinOpExpr(And, _, _))
  }

  private def simplifyClause(clause: Expression) = clause match {
    case BinOpExpr(Or, l, r) =>
      (simplify(l), simplify(r)) match {
        case (Some(l1), Some(r1)) =>
          Some(BinOpExpr(Or, l1, r1))
        case _ =>
          // one side of Or is true, so the entire expression is true
          None
      }
    case _ =>
      if (implies(clause))
        None
      else
        Some(clause)
  }

  private def implies(clause: Expression): Boolean = clause match {
    case Literal(true, BoolType) =>
      true
    case _ =>
      val extractedConstraints = extractConstraints(clause)
      extractedConstraints.exists(implies)
  }
  private def implies(constraint: Constraint): Boolean = {
    val implyingOps = constraint.op :: (constraint.op match {
      case LessEq => List(Less, Eq)
      case GreaterEq => List(Greater, Eq)
      case _ => Nil
    })
    implyingOps.exists { op => asMap(constraint.lhs)(op).contains(constraint.rhs) }
  }
}

object ConstraintSet {
  val empty = new ConstraintSet(Map.empty.withDefaultValue(Map.empty.withDefaultValue(Set.empty)))
}
