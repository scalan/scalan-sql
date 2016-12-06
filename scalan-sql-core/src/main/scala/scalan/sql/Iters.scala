package scalan.sql

import java.lang.reflect.Method

import scalan._
import scala.reflect.runtime.universe._
import scalan.sql.parser.SqlAST.{ComparisonOp, Index, SortDirection, Table}

trait Iters extends ScalanDsl {
  self: ItersDsl with ScalanSql =>

  type RIter[A] = Rep[Iter[A]]

  trait Iter[Row] extends Def[Iter[Row]] {
    def eRow: Elem[Row]

    def map[B](f: Rep[Row => B]): RIter[B] = delayInvoke

    // second parameter is used to create an initial value
    def mapU[B](f: Rep[((B, Row)) => Unit], eB: Elem[B]): RIter[B] = delayInvoke

    def flatMap[B](f: Rep[Row => Iter[B]]): RIter[B] = delayInvoke

    def filter(f: Rep[Row => Boolean]): RIter[Row] = delayInvoke

    def takeWhile(f: Rep[Row => Boolean]): RIter[Row] = delayInvoke

    def isEmpty: Rep[Boolean] = delayInvoke

    def reduce[B](f: Rep[((B, Row)) => B], init: Rep[Thunk[B]]): RIter[B] = delayInvoke

    def reduceU[B](f: Rep[((B, Row)) => Unit], init: Rep[Thunk[B]]): RIter[B] = delayInvoke

    /**
      * @param packKey serialize key into string preserving uniqueness
      * @tparam K key type
      * @return iterator with structures of type {key: K, val: V}
      */
    def mapReduce[K, V](mapKey: Rep[Row => K],
                        packKey: Rep[Row => String],
                        newValue: Rep[Thunk[V]],
                        reduceValue: Rep[((V, Row)) => V]
                       ): RIter[Struct] = delayInvoke

    def mapReduceU[K, V](mapKey: Rep[Row => K],
                         packKey: Rep[Row => String],
                         newValue: Rep[Thunk[V]],
                         reduceValue: Rep[((V, Row)) => Unit]
                        ): RIter[Struct] = delayInvoke

    /** Use instead of mapReduce when input is sorted by some prefix of the grouping.
      * @param prefixComparator returns true when two rows belong to the same group
      * @tparam K will be a pair of structs: group key and hash key (second may be empty)
      */
    def partialMapReduce[K, V](prefixComparator: Rep[((Row, Row)) => Boolean],
                               mapKey: Rep[Row => K],
                               packKey: Rep[Row => String],
                               newValue: Rep[Thunk[V]],
                               reduceValue: Rep[((V, Row)) => V]
                              ): RIter[Struct] = delayInvoke

    def partialMapReduceU[K, V](prefixComparator: Rep[((Row, Row)) => Boolean],
                                mapKey: Rep[Row => K],
                                packKey: Rep[Row => String],
                                newValue: Rep[Thunk[V]],
                                reduceValue: Rep[((V, Row)) => Unit]
                               ): RIter[Struct] = delayInvoke

    // sort takes a less-than predicate, sortBy takes an Ordering.compare-like function
    def sort(comparator: Rep[((Row, Row)) => Boolean]): RIter[Row] = delayInvoke
    def sortBy(comparator: Rep[((Row, Row)) => Int]): RIter[Row] = delayInvoke

    /**
      * Use when input is already sorted on some prefix of desired ordering
      * @param prefixComparator return true when both rows belong to the same group
      * @param suffixComparator compare rows within one group, return true if the first is less than the second
      */
    def partialSort(prefixComparator: Rep[((Row, Row)) => Boolean], suffixComparator: Rep[((Row, Row)) => Boolean]): RIter[Row] =
      delayInvoke

    // if `leftIsOuter` is true, `other` will be hashed; otherwise, `this` will be
    def join[B, Key](other: RIter[B], thisKey: Rep[Row => Key], otherKey: Rep[B => Key], cloneOther: Rep[B => B]/*, joinType: JoinType*/): RIter[(Row, B)] = delayInvoke

    def toArray: Arr[Row] = delayInvoke

    // always called with Clone and equivalent to `map`, but semantically different
    def materialize(f: Rep[Row => Row]): RIter[Row] = delayInvoke
  }
  trait IterCompanion {
    def empty[Row: Elem]: Rep[Iter[Row]] = externalMethod("IterCompanion", "empty")
  }

  trait CursorIter[Row] extends Iter[Row] {
    def eRow: Elem[Row]

    def table: Rep[Table]
    def scanId: Rep[Int]
    def direction: Rep[SortDirection]
    def fakeDep: Rep[Unit]
    def kernelInput: Rep[KernelInput]

    def fromKeyWhile(keyValues: Rep[Array[Any]], operation: ComparisonOp, takeWhilePred: Rep[Row => Boolean]): Rep[CursorIter[Row]] = delayInvoke
  }

  abstract class TableIter[Row](val table: Rep[Table], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends CursorIter[Row] {
    def byRowids[B](iter: RIter[B], f: Rep[B => Rowid]): RIter[Row] = delayInvoke
  }

  abstract class IndexIter[Row](val table: Rep[Table], val index: Rep[Index], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends CursorIter[Row]
}

// TODO add rewrite rules map(IdentityLambda) etc.
trait ItersDsl extends impl.ItersAbs { self: ScalanSql =>

  trait IterFunctor extends Functor[Iter] {
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Iter[T]]
    def lift[T](implicit eT: Elem[T]) = element[Iter[T]]
    def unlift[T](implicit eFT: Elem[Iter[T]]) = eFT.asInstanceOf[IterElem[T,_]].eRow
    def getElem[T](fa: Rep[Iter[T]]) = rep_getElem(fa)
    def unapply[A](e: Elem[_]) = e match {
      case ae: IterElem[_, _] => Some(ae.asElem[Iter[A]])
      case _ => None
    }
    def map[A:Elem,B:Elem](xs: Rep[Iter[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))
  }
  implicit val iterContainer: Functor[Iter] = new IterFunctor {}

  implicit def iterElemExtensions[A](ie: Elem[Iter[A]]) = ie.asInstanceOf[IterElem[A, Iter[A]]]

  def advanceIter[Row](iter: RIter[Row]): Rep[(Row, Iter[Row])] = ???

  def clone[A](x: Rep[A]): Rep[A] = ???

  // TODO remove when SqlIterBridge is removed
  def cloneFun[A](implicit eA: Elem[A]) = fun[A, A] { clone(_) }
}

trait ItersDslStd extends impl.ItersStd { self: ScalanSqlStd =>
}

trait ItersDslExp extends impl.ItersExp { self: ScalanSqlExp =>
  private[this] var advanceIterCounter = 0

  case class AdvanceIter[Row](iter: RIter[Row], n: Int) extends {
    implicit val eRow = iter.elem.eRow
  } with BaseDef[(Row, Iter[Row])]

  case class Clone[A](x: Rep[A]) extends BaseDef[A]()(x.elem)

  override def clone[A](x: Rep[A]): Rep[A] = Clone(x)

  override def advanceIter[Row](iter: RIter[Row]): Rep[(Row, Iter[Row])] = {
    advanceIterCounter += 1
    AdvanceIter(iter, advanceIterCounter)
  }


  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
    case iterElem: IterElem[_, _] =>
      m.getName match {
        case "filter" | "takeWhile" | "fromKeyWhile" | "sort" | "sortBy" | "materialize" | "seekIndex" | "partialSort" | "byRowids" =>
          val eRow = receiver.elem.asInstanceOf[IterElem[_, _]].eRow
          iterElement(eRow)
        case "map" =>
          val f = args(0).asInstanceOf[Exp[_]]
          val eB = f.elem.asInstanceOf[FuncElem[_, _]].eRange
          iterElement(eB)
        case "mapU" =>
          val f = args(0).asInstanceOf[Exp[_]]
          val eB = f.elem.asInstanceOf[FuncElem[_, _]].eDom.asInstanceOf[PairElem[_, _]].eFst
          iterElement(eB)
        case "flatMap" =>
          val f = args(0).asInstanceOf[Exp[_]]
          val eIterB = f.elem.asInstanceOf[FuncElem[_, _]].eRange
          eIterB
        case "mapReduce" | "mapReduceU" =>
          val mapKey = args(0).asInstanceOf[Exp[_]]
          val newValue = args(2).asInstanceOf[Exp[_]]
          val eK = mapKey.elem.asInstanceOf[FuncElem[_, _]].eRange
          val eV = newValue.elem.asInstanceOf[ThunkElem[_]].eItem
          iterElement(keyValElem(eK, eV))
        case "partialMapReduce" | "partialMapReduceU" =>
          val mapKey = args(1).asInstanceOf[Exp[_]]
          val newValue = args(3).asInstanceOf[Exp[_]]
          val eK = mapKey.elem.asInstanceOf[FuncElem[_, _]].eRange
          val eV = newValue.elem.asInstanceOf[ThunkElem[_]].eItem
          iterElement(keyValElem(eK, eV))
        case "join" =>
          val other = args(0).asInstanceOf[Exp[_]]
          val eB = other.elem.asInstanceOf[IterElem[_, _]].eRow
          val eA = iterElem.eRow
          iterElement(pairElement(eA, eB))
        case "onlyElement" | "next" =>
          iterElem.eRow
        case _ => super.getResultElem(receiver, m, args)
      }
    case _ =>
      super.getResultElem(receiver, m, args)
  }

  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
    case IterMethods.map(iter, Def(IdentityLambda())) =>
      iter
    case IterMethods.map(ys @ Def(d2), f: Rep[Function1[a, b]] @unchecked) =>
      d2.asDef[Iter[a]] match {
        case IterMethods.map(xs: Rep[Iter[c]] @unchecked, g) => //TODO if hasSingleUsage(ys)
          val g1 = g.asRep[c => a]
          implicit val eB = f.elem.eRange
          implicit val eC = xs.elem.asInstanceOf[IterElem[c,_]].eRow
          val res = xs.map { x: Rep[c] => f(g1(x)) }
          res
        case _ => super.rewriteDef(d)
      }
    case IterMethods.filter(iter: RIter[a], Def(ConstantLambda(c))) =>
      IF (c) THEN iter ELSE Iter.empty(iter.selfType1.eRow)
    case IterMethods.takeWhile(iter: RIter[a], Def(ConstantLambda(c))) =>
      IF (c) THEN iter ELSE Iter.empty(iter.selfType1.eRow)
    case IterMethods.filter(iter1 @ Def(IterMethods.takeWhile(iter, f)), g) if f == g =>
      iter1
    case IterMethods.takeWhile(iter1 @ Def(IterMethods.filter(iter, f)), g) if f == g =>
      iter1
      // TODO do we need rules for filter/filter, takeWhile/takeWhile, range/takeWhile fusion?

    case _ => super.rewriteDef(d)
  }
}
