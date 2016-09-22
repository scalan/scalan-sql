package scalan.sql

import java.lang.reflect.Method

import scalan._
import scalan.sql.parser.SqlAST._

trait Scannables extends ScalanDsl {
  self: ScannablesDsl with ScalanSql =>

  type RScannable[A] = Rep[Scannable[A]]

  trait Scannable[Row] extends Def[Scannable[Row]] {
    def eRow: Elem[Row]

    def sourceIter(): Rep[CursorIter[Row]]

    def fullScan(): RRelation[Row] = IterBasedRelation(sourceIter())(eRow)
  }

  abstract class TableScannable[Row](val table: Rep[Table], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends Scannable[Row] {
    override def sourceIter() = TableIter(table, scanId, direction, fakeDep, kernelInput)
  }

  abstract class IndexScannable[Row](val table: Rep[Table], val index: Rep[Index], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends Scannable[Row] {
    def isCovering: Boolean = Scannables.this.isCovering(table.asValue, index.asValue, eRow)

    override def sourceIter() = IndexIter(table, index, scanId, direction, fakeDep, kernelInput)

    // FIXME assumes all columns in index are ASC
    def search(bounds: SearchBounds): RRelation[Row] = {
      val index0 = index.asValue
      val direction0 = direction.asValue

      def inverseIfDescending(op: ComparisonOp) = op.inverseIfDescending(direction0)

      val (startBound, endBound) = direction0 match {
        case Ascending => (bounds.lowerBound, bounds.upperBound)
        case Descending => (bounds.upperBound, bounds.lowerBound)
      }
      val fixedValues = bounds.fixedValues
      val (keyValues, startOpIfAscending) = startBound match {
        case None =>
          (fixedValues, GreaterEq)
        case Some(Bound(value, isInclusive)) =>
          (fixedValues :+ value, if (isInclusive) GreaterEq else Greater)
      }
      val startOp = inverseIfDescending(startOpIfAscending)
      val test = fun[Row, Boolean] { _x =>
        val x = _x.asRep[Struct]

        val firstCondition = endBound match {
          case None => toRep(true)
          case Some(Bound(value, isInclusive)) =>
            val column = index0.columns(fixedValues.length)
            val y = field(x, column.name)
            val endOp = inverseIfDescending(if (isInclusive) LessEq else Less)

            comparisonOp(endOp, y, value)
        }

        (index0.columns, fixedValues).zipped.foldLeft(firstCondition) {
          case (cond, (column, value)) =>
            val y = field(x, column.name)
            cond && comparisonOp(Eq, y, value)
        }
      }

      val boundedIter0 = if (keyValues.nonEmpty) {
        val repKeyValues = SArray.fromSyms(keyValues.asInstanceOf[List[Rep[Any]]])(AnyElement)
        sourceIter().seekIndex(repKeyValues, startOp)
      } else
        sourceIter()
      val boundedIter = boundedIter0.takeWhile(test)
      IterBasedRelation(boundedIter)
    }
  }
}

trait ScannablesDsl extends impl.ScannablesAbs { self: ScalanSql =>
  implicit def ScannableElemExtensions[A](ie: Elem[Scannable[A]]) = ie.asInstanceOf[ScannableElem[A, Scannable[A]]]

  case class Bound(value: Rep[_], isInclusive: Boolean)

  case class SearchBounds(fixedValues: List[Rep[_]], lowerBound: Option[Bound], upperBound: Option[Bound]) {
    def addFixedValue(value: Rep[_]) = copy(fixedValues = value :: fixedValues)
  }

  object SearchBounds {
    def fixedValue(value: Rep[_]) = SearchBounds(List(value), None, None)
    def range(lowerBound: Option[Bound], upperBound: Option[Bound]) = SearchBounds(Nil, lowerBound, upperBound)
  }
}

trait ScannablesDslStd extends impl.ScannablesStd { self: ScalanSqlStd =>
}

trait ScannablesDslExp extends impl.ScannablesExp { self: ScalanSqlExp =>
  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
    case elem: ScannableElem[_, _] =>
      m.getName match {
        case "fullScan" | "search" =>
          relationElement(elem.eRow)
        case _ => super.getResultElem(receiver, m, args)
      }
    case _ =>
      super.getResultElem(receiver, m, args)
  }

  override def formatConst(x: Any) = x match {
    case x: Table => s"Table ${x.name}"
    case x: Index => s"Index ${x.name} ON ${x.tableName}"
    case _ => super.formatConst(x)
  }
}
