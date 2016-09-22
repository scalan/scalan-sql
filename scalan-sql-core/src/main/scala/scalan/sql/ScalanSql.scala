package scalan.sql

import java.sql.{Date, Time, Timestamp}

import scalan._
import scalan.sql.parser.SqlAST._

/**
  * Use this traits for the top level scalan.sql extensions, customizations and overrides
  */
trait ScalanSql extends ScalanDsl with ScannablesDsl with KernelInputsDsl with ItersDsl with RelationsDsl {
  implicit val DateElement: Elem[Date] = new BaseElem(new Date(0))
  implicit val DateOrdering: Ordering[Date] = Ordering.by(_.getTime)

  implicit val TimeElement: Elem[Time] = new BaseElem(new Time(0))
  implicit val TimeOrdering: Ordering[Time] = Ordering.by(_.getTime)

  implicit val TimestampElement: Elem[Timestamp] = new BaseElem(new Timestamp(0))
  implicit val TimestampOrdering: Ordering[Timestamp] = Ordering.by(_.getTime)

  implicit val BooleanNumeric: Numeric[Boolean] = new Numeric[Boolean] {
    def plus(x: Boolean, y: Boolean): Boolean = ???
    def minus(x: Boolean, y: Boolean): Boolean = ???
    def times(x: Boolean, y: Boolean): Boolean = ???
    def negate(x: Boolean): Boolean = ???
    def fromInt(x: Int): Boolean = x match {
      case 0 => false
      case 1 => true
    }
    def toInt(x: Boolean): Int = if (x) 1 else 0
    def toLong(x: Boolean): Long = toInt(x)
    def toFloat(x: Boolean): Float = toInt(x)
    def toDouble(x: Boolean): Double = toInt(x)
    def compare(x: Boolean, y: Boolean) = x compare y
  }

  val QueryTextKey = MetaKey[String]("queryText")

  implicit val sqlOperatorElem = new BaseElem[Operator](null)
  implicit val sqlExpressionElem = new BaseElem[Expression](null)
  implicit val tableElem = new BaseElem[Table](null)
  implicit val indexElem = new BaseElem[Index](null)
  implicit val comparisonOpElem = new BaseElem[ComparisonOp](null)
  implicit val directionElem = new BaseElem[SortDirection](Ascending)
  val SqlOperatorKey = MetaKey[Operator]("sql-origin-operator")
  val SqlExpressionKey = MetaKey[Expression]("sql-origin-expression")
  val CandidateIndicesKey = MetaKey[((Table, List[Index]), Int)]("sql-candidate-indices")

  // same as ToString, but without rewriting (x: String).toString => x
  // this way it can be used to generate to_lua_string in the end
  def toPlatformString[A](x: Rep[A]): Rep[String]

  val MaterializeKey = MetaKey[Unit]("materialize")

  val keyFld = "key"
  val valFld = "val"
  def keyValElem(eK: Elem[_], eV: Elem[_]) = structElement(Seq(keyFld -> eK, valFld -> eV))

  def pack[A](x: Rep[A]): Rep[String]

  def getOrdering[T](e: Elem[T]): Ordering[T] = (e.asInstanceOf[TypeDesc] match {
    case IntElement => implicitly[Ordering[Int]]
    case LongElement => implicitly[Ordering[Long]]
    case DoubleElement => implicitly[Ordering[Double]]
    case CharElement => implicitly[Ordering[Char]]
    case StringElement => implicitly[Ordering[String]]
    case DateElement => implicitly[Ordering[Date]]
    case PairElem(eFst: Elem[a], eSnd: Elem[b]) =>
      val ordA: Ordering[a] = getOrdering(eFst)
      val ordB: Ordering[b] = getOrdering(eSnd)
      Ordering.Tuple2(ordA, ordB)
    case _ => ???(s"Don't know how to create Ordering for $e")
  }).asInstanceOf[Ordering[T]]

  def getNumeric[T](e: Elem[T]): Numeric[T] = (e.asInstanceOf[TypeDesc] match {
    case IntElement => implicitly[Numeric[Int]]
    case DoubleElement => implicitly[Numeric[Double]]
    case LongElement => implicitly[Numeric[Long]]
    case BooleanElement => BooleanNumeric
    //    case DateElement => implicitly[Numeric[Date]]
    case _ => ???(s"Don't know how to create Numeric for $e")
  }).asInstanceOf[Numeric[T]]

  private case class LRHS[A](l: Rep[A], r: Rep[A], elem: Elem[A])

  // brings l and r to the same type
  private def widen[A, B](l: Rep[A], r: Rep[B]): LRHS[_] = {
    implicit val eL = rep_getElem(l)
    implicit val eR = rep_getElem(r)
    if (eL == eR)
      LRHS(l, r.asRep[A], eL)
    else
      (eL.asInstanceOf[TypeDesc], eR.asInstanceOf[TypeDesc]) match {
        // Should handle two structs with same field names?
        case (StructElem(_, fields), _) =>
          if (fields.length == 1) {
            val l1 = field(l.asRep[Struct], 0)
            widen(l1, r)
          } else {
            !!!(s"Arithmetic operation on a multi-field struct $l: ${eL.name}", l, r)
          }
        case (_, StructElem(_, fields)) =>
          if (fields.length == 1) {
            val r1 = field(r.asRep[Struct], 0)
            widen(l, r1)
          } else {
            !!!(s"Arithmetic operation on a multi-field struct $r: ${eR.name}", l, r)
          }
        // zipWith for two iterators?
        case (eL: RelationElem[a, _], _) =>
          // TODO verify that l itself isn't used elsewhere, same below
          val l1 = l.asRep[Relation[a]].onlyValue()
          widen(l1, r)
        case (_, eR: RelationElem[b, _]) =>
          val r1 = r.asRep[Relation[b]].onlyValue()
          widen(l, r1)
        case (DoubleElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Double], r.toDouble, DoubleElement)
        case (_, DoubleElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toDouble, r.asRep[Double], DoubleElement)
        case (LongElement, FloatElement) =>
          LRHS(l.asRep[Long].toDouble, r.asRep[Float].toDouble, DoubleElement)
        case (FloatElement, LongElement) =>
          LRHS(l.asRep[Float].toDouble, r.asRep[Long].toDouble, DoubleElement)
        case (LongElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Long], r.toLong, LongElement)
        case (_, LongElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toLong, r.asRep[Long], LongElement)
        case (FloatElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Float], r.toFloat, FloatElement)
        case (_, FloatElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toFloat, r.asRep[Float], FloatElement)
        case (IntElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Int], r.toInt, IntElement)
        case (_, IntElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toInt, r.asRep[Int], IntElement)
        // Dates and times are represented as strings in SQLite
        case (DateElement | TimeElement | TimestampElement, StringElement) =>
          LRHS(l.asRep[String], r.asRep[String], StringElement)
        case (StringElement, DateElement | TimeElement | TimestampElement) =>
          LRHS(l.asRep[String], r.asRep[String], StringElement)
        case (StringElement, CharElement) =>
          LRHS(l.asRep[String], r.toStringRep, StringElement)
        case (CharElement, StringElement) =>
          LRHS(l.toStringRep, r.asRep[String], StringElement)
        case _ =>
          !!!(s"No common numeric type for $l: ${eL.name} and $r: ${eR.name}", l, r)
      }
  }

  def numOp[A, B](l: Rep[A], r: Rep[B])(f: (Numeric[A], Elem[A]) => BinOp[A, A]) = {
    val LRHS(l1, r1, elem) = widen(l, r)
    val num = getNumeric(elem)
    // works by type erasure
    f(num.asInstanceOf[Numeric[A]], elem.asElem[A])(l1.asRep[A], r1.asRep[A])
  }

  def ordOp[A, B, C](l: Rep[A], r: Rep[B])(f: (Ordering[A], Elem[A]) => BinOp[A, C]) = {
    val LRHS(l1, r1, elem) = widen(l, r)
    val ord = getOrdering(elem)
    f(ord.asInstanceOf[Ordering[A]], elem.asElem[A])(l1.asRep[A], r1.asRep[A])
  }

  def comparisonOp[A, B](op: ComparisonOp, l: Rep[A], r: Rep[B]): Rep[Boolean] = op match {
    case Eq | Is =>
      ordOp(l, r)((_, _) => Equals())
    case Less =>
      ordOp(l, r)((ord, _) => OrderingLT(ord))
    case LessEq =>
      ordOp(l, r)((ord, _) => OrderingLTEQ(ord))
    case Greater =>
      ordOp(l, r)((ord, _) => OrderingGT(ord))
    case GreaterEq =>
      ordOp(l, r)((ord, _) => OrderingGTEQ(ord))
  }

  def isCovering(table: Table, index: Index, eRow: Elem[_]) = eRow match {
    case se: StructElem[_] =>
      val indexColumns = index.columns.map(_.name) ++ rowidColumn(table)

      se.fieldNames.forall(indexColumns.contains)
  }

  def rowidColumn(table: Table) = table.columns.find {
    c => c.ctype == IntType && c.constraints.exists(_.isInstanceOf[PrimaryKeyC])
  }.map(_.name)
}

trait ScalanSqlStd extends ScalanDslStd with ScannablesDslStd with KernelInputsDslStd with ItersDslStd with RelationsDslStd with ScalanSql {
  override def pack[A](x: A): String = x.toString
}
trait ScalanSqlExp extends ScalanDslExp with ScannablesDslExp with KernelInputsDslExp with ItersDslExp with RelationsDslExp with ScalanSql with SqlSlicing {
  // stops us from recalculating plans when rewriting functions
  override def unfoldLambda[A,B](lam: Lambda[A,B], x: Exp[A]): Exp[B] = mirrorApply(lam, x)

  def toPlatformString[A](x: Rep[A]): Rep[String] = ToString1[A]()(x)
  case class ToString1[A]() extends UnOp[A, String]("toPlatformString", _.toString)

  override def pack[A](x: Rep[A]) = x.elem.asInstanceOf[TypeDesc] match {
    case StructElem(_, fieldElems) =>
      val separator = toRep("|||")
      fieldElems match {
        case Seq() =>
          StringObject.empty
        case _ =>
          fieldElems.map { case (name, _) => pack(field(x.asRep[Struct], name)) }.reduce(_ + separator + _)
      }
    case _ =>
      toPlatformString(x)
  }

  // ctx ensures parameters are read inside lambdas only
  case class Parameter[A](index: Int, ctx: Exp[_], value: Any)(implicit val selfType: Elem[A]) extends Def[A]

  case class ExtraDeps(deps: Seq[Rep[_]]) extends BaseDef[Unit] {
    override def toString = s"ExtraDeps(${deps.mkString(", ")})"
  }
  def extraDeps(deps: Rep[_]*) = reifyObject(ExtraDeps(deps))
}
