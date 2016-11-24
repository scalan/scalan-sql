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

    def byRowids[B](relation: RRelation[B], f: Rep[B => Rowid]): RRelation[Row] = {
      val iter = sourceIter().byRowids(relation.iter, f)
      iterBasedRelation(iter)
    }
  }

  abstract class IndexScannable[Row](val table: Rep[Table], val index: Rep[Index], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends Scannable[Row] {
    override def sourceIter() = IndexIter(table, index, scanId, direction, fakeDep, kernelInput)

    // FIXME assumes all columns in index are ASC
    def search(bounds: SearchBounds): RRelation[Row] = {
      val index0 = index.asValue
      val fixedValues = bounds.fixedValues

      def keyArray(values: List[Rep[_]]) =
        SArray.fromSyms(values.asInstanceOf[List[Rep[Any]]])(AnyElement)

      val iter = if (fixedValues.length == index0.columns.length && index0.isUnique) {
        val repFixedValues = keyArray(fixedValues)
        sourceIter().uniqueByKey(repFixedValues)
      } else {
        val direction0 = direction.asValue

        def inverseIfDescending(op: ComparisonOp) = op.inverseIfDescending(direction0)

        val (startBound, endBound) = direction0 match {
          case Ascending => (bounds.lowerBound, bounds.upperBound)
          case Descending => (bounds.upperBound, bounds.lowerBound)
        }

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

        if (keyValues.nonEmpty) {
          val repKeyValues = keyArray(keyValues)
          sourceIter().fromKeyWhile(repKeyValues, startOp, test)
        } else
          sourceIter().takeWhile(test)
      }
      // if bounds.isEmpty this is the same as fullScan()
      IterBasedRelation(iter)
    }
  }
}

trait ScannablesDsl extends impl.ScannablesAbs { self: ScalanSql =>
  implicit def ScannableElemExtensions[A](ie: Elem[Scannable[A]]) = ie.asInstanceOf[ScannableElem[A, Scannable[A]]]

  case class Bound(value: Rep[_], isInclusive: Boolean)

  case class SearchBounds(fixedValues: List[Rep[_]], lowerBound: Option[Bound], upperBound: Option[Bound]) {
    def isEmpty = fixedValues.isEmpty && lowerBound.isEmpty && upperBound.isEmpty
    def addFixedValue(value: Rep[_]) = copy(fixedValues = value :: fixedValues)
  }

  object SearchBounds {
    val empty = SearchBounds(Nil, None, None)
    def range(lowerBound: Option[Bound], upperBound: Option[Bound]) = SearchBounds(Nil, lowerBound, upperBound)
  }
}

trait ScannablesDslStd extends impl.ScannablesStd { self: ScalanSqlStd =>
}

trait ScannablesDslExp extends impl.ScannablesExp { self: ScalanSqlExp =>
  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
    case elem: ScannableElem[_, _] =>
      m.getName match {
        case "fullScan" | "search" | "byRowids" =>
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
