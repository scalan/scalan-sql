package scalan.sql

import java.lang.reflect.Method

import scalan._
import scalan.sql.parser.SqlAST._

trait Scannables extends ScalanDsl {
  self: ScannablesDsl with ScalanSql =>

  type RScannable[A, B] = Rep[Scannable[A, B]]

  trait Scannable[TableRow, SourceRow] extends Def[Scannable[TableRow, SourceRow]] {
    def eTableRow: Elem[TableRow]
    def eSourceRow: Elem[SourceRow]
    def table: Rep[Table]
    def scanId: Rep[Int]
    def fakeDep: Rep[Unit]
    def kernelInput: Rep[KernelInput]
    def isCovering: Boolean

    def sourceIter(): Either[Rep[CursorIter[TableRow]], (Rep[IndexIter[SourceRow]], Rep[SourceRow => Long])]

    def mkIter(f: Rep[CursorIter[TableRow]] => Rep[Iter[TableRow]])(g: Rep[IndexIter[SourceRow]] => Rep[Iter[SourceRow]]): RIter[TableRow] = {
      sourceIter() match {
        case Left(tableIter) =>
          f(tableIter)
        case Right((indexIter, rowidGetter)) =>
          val iter1 = g(indexIter)
          if (isCovering) {
            // TODO may have extra field for rowid, think how to remove it!
            iter1.asRep[Iter[TableRow]]
          } else {
            TableIterByRowids(table.asValue, scanId.asValue, iter1, rowidGetter)(eTableRow, eSourceRow)
          }
      }
    }

    def fullScan(): RRelation[TableRow] = IterBasedRelation(mkIter(identity)(identity))(eTableRow)
  }

  abstract class TableScannable[Row](val table: Rep[Table], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends Scannable[Row, Row] {
    override def eTableRow = eRow
    override def eSourceRow = eRow
    override def isCovering = true
    override def sourceIter() = Left(TableIter(table, scanId, direction, fakeDep, kernelInput))
  }

  abstract class IndexScannable[Row, IndexRow](val table: Rep[Table], val index: Rep[Index], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row], val eIndexRow: Elem[IndexRow]) extends Scannable[Row, IndexRow] {
    override def eSourceRow = eIndexRow
    override def isCovering: Boolean = Scannables.this.isCovering(table.asValue, index.asValue, eRow)

    override def sourceIter() = {
      if (isCovering) {
        Left(IndexIter(table, index, scanId, direction, fakeDep, kernelInput)(eRow))
      } else {
        val indexIter = IndexIter(table, index, scanId, direction, fakeDep, kernelInput)(eIndexRow)
        val index0 = index.asValue
        val indexColumnNames = index0.columns.map(_.name)
        val rowidFieldNameInIndexRow = rowidColumn(table.asValue).filter(indexColumnNames.contains) match {
          case Some(name) =>
            name
          case None =>
            indexColumnNames.last
        }
        val f = fun[IndexRow, Long] { x =>
          val rowid = x.asRep[Struct].getUntyped(rowidFieldNameInIndexRow)
          (rep_getElem(rowid): Elem[_]) match {
            case IntElement =>
              rowid.asRep[Int].toLong
            case LongElement =>
              rowid.asRep[Long]
            case elem =>
              !!!(s"rowid in index ${index0.name} on table ${index0.tableName} found under name ${rowidFieldNameInIndexRow}, but its type is $elem instead of INTEGER or BIGINT")
          }
        }
        Right((indexIter, f))
      }
    }

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
        mkIter(_.seekIndex(repKeyValues, startOp))(_.seekIndex(repKeyValues, startOp))
      } else
        mkIter(identity)(identity)
      val boundedIter = boundedIter0.takeWhile(test)
      // if bounds.isEmpty this is the same as fullScan()
      IterBasedRelation(boundedIter)
    }
  }
}

trait ScannablesDsl extends impl.ScannablesAbs { self: ScalanSql =>
  implicit def ScannableElemExtensions[A, B](ie: Elem[Scannable[A, B]]) =
    ie.asInstanceOf[ScannableElem[A, B, Scannable[A, B]]]

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
    case elem: ScannableElem[_, _, _] =>
      m.getName match {
        case "fullScan" | "search" =>
          relationElement(elem.eTableRow)
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
