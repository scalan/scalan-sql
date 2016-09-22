package scalan.sql

import java.lang.reflect.Method

import scalan._
import scalan.common.Lazy
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

    // sort takes a less-than predicate, sortBy takes an Ordering.compare-like function
    def sort(comparator: Rep[((Row, Row)) => Boolean]): RIter[Row] = delayInvoke
    def sortBy(comparator: Rep[((Row, Row)) => Int]): RIter[Row] = delayInvoke

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

    def seekIndex(keyValues: Rep[Array[Any]], operation: ComparisonOp): Rep[CursorIter[Row]] = delayInvoke
  }

  abstract class TableIter[Row](val table: Rep[Table], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends CursorIter[Row]

  // TODO how to distinguish covering indices?
  abstract class IndexIter[Row](val table: Rep[Table], val index: Rep[Index], val scanId: Rep[Int], val direction: Rep[SortDirection], val fakeDep: Rep[Unit], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends CursorIter[Row] {
    def isCovering: Boolean = Iters.this.isCovering(table.asValue, index.asValue, eRow)
  }
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

  case class IterMarking[T](itemsPath: KeyPath, override val innerMark: SliceMarking[T]) extends SliceMarking1[T,Iter](innerMark) {
    implicit val eItem = innerMark.elem
    val elem = element[Iter[T]]
    def children: Seq[SliceMarking[_]] = Seq(innerMark)
    def meet(other: SliceMarking[Iter[T]]) = ???
    def join(other: SliceMarking[Iter[T]]) = other match {
      case am: IterMarking[T] @unchecked if am.itemsPath == itemsPath =>
        IterMarking(itemsPath, innerMark.join(am.innerMark))
      case am: IterMarking[T] @unchecked if am.itemsPath == KeyPath.None =>
        this
      case am: IterMarking[T] @unchecked if this.itemsPath == KeyPath.None =>
        other
    }
    def >>[R](other: SliceMarking[R]): SliceMarking[Iter[T]] = other match {
      case am: IterMarking[_] if am.itemsPath == itemsPath =>
        IterMarking(itemsPath, innerMark >> am.innerMark)
      case am: IterMarking[_] if am.itemsPath == KeyPath.None =>
        IterMarking(KeyPath.None, innerMark >> am.innerMark)
    }
    def nonEmpty = innerMark.nonEmpty && (!itemsPath.isNone)
    def isIdentity = itemsPath.isAll && innerMark.isIdentity
    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = key match {
      case KeyPath.All if inner.elem == eItem =>
        IterMarking[T](key, inner.asMark[T])
    }
    def projectToExp(xs: Exp[Iter[T]]): Exp[_] = itemsPath match {
      case KeyPath.All =>
        assert(xs.elem == this.elem)
        reifyObject(UnpackSliced(xs, this))
      case KeyPath.None =>
        Iter.empty[T]
      case _ =>
        !!!(s"itemsPath = $itemsPath")
    }
    val projectedElem = iterElement(innerMark.projectedElem)
    def makeSlot = {
      SlicedIter(fresh(Lazy(projectedElem)), innerMark)
    }
    def set(slot: Exp[Iter[T]], value: Exp[_]) = slot match {
      case Def(iter: SlicedIter[T,a]@unchecked) =>
        SlicedIter(value.asRep[Iter[a]], iter.innerMark)
      case _ =>
        setInvalid(slot, value)
    }
  }

  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
    case iterElem: IterElem[_, _] =>
      m.getName match {
        case "filter" | "takeWhile" | "sort" | "sortBy" | "materialize" | "seekIndex" =>
          receiver.elem
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

  override def rewriteDef[T](d: Def[T]): Exp[_] = { import KeyPath._; d match {
    case IterMethods.filter(
            IsSliced(xs: RIter[s]@unchecked, im @ IterMarking(All, mA: SliceMarking[a])), _p) =>
      val p = _p.asRep[a => Boolean]
      val sp = sliceIn(p, mA).asRep[s => Boolean]
      val eS = xs.elem.eRow
      assert(eS == sp.elem.eDom, s"${eS} == ${sp.elem.eDom}")
      Sliced(xs.filter(sp), im.asMark[Iter[a]])

    case IterMethods.takeWhile(
            IsSliced(xs: RIter[s]@unchecked, im @ IterMarking(All, mA: SliceMarking[a])), _p) =>
      val p = _p.asRep[a => Boolean]
      val sp = sliceIn(p, mA).asRep[s => Boolean]
      val eS = xs.elem.eRow
      assert(eS == sp.elem.eDom, s"${eS} == ${sp.elem.eDom}")
      Sliced(xs.takeWhile(sp), im.asMark[Iter[a]])

    case CursorIterMethods.seekIndex(
            IsSliced(xs: Rep[CursorIter[s]] @unchecked, im @ IterMarking(All, mA: SliceMarking[a])), keyValues, op) =>
      Sliced(xs.seekIndex(keyValues, op), im.asMark[Iter[a]])

    case IterMethods.map(
            IsSliced(_xs, IterMarking(All, mA: SliceMarking[a])),
            _f: RFunc[_,b]@unchecked) =>
      val f = _f.asRep[a => b]
      val fm @ FuncMarking(_, mB) = sliceAnalyzer.getMark(f)
      fm.projectedElem match {
        case fe: FuncElem[s,t] =>
          val xs = _xs.asRep[Iter[s]]
          val fs = sliceFunc(f, FuncMarking(mA, mB.asMark[b])).asRep[s => t]
          assert(xs.elem.eRow == fs.elem.eDom, s"${xs.elem.eRow} != ${fs.elem.eDom}")
          assert(xs.elem.eRow == mA.projectedElem, s"${xs.elem.eRow} == ${mA.projectedElem}")
          assert(fs.elem.eRange == mB.projectedElem, s"${fs.elem.eRange} == ${mB.projectedElem}")
          Sliced(xs.map(fs), IterMarking(All, mB))
      }

    case IterMethods.mapReduce(
            IsSliced(xs: RIter[s]@unchecked, IterMarking(KeyPath.All, sm: SliceMarking[a])),
            _mapKey: RFunc[_,k]@unchecked, _packKey, newValue: Th[v] @unchecked, _reduceValue) =>
      val mapKey = _mapKey.asRep[a => k]
      val packKey = _packKey.asRep[a => String]
      val eV = newValue.elem.eItem
      val reduceInMarking = PairMarking(eV.toMarking, sm)
      val sReduce = sliceIn(_reduceValue.asRep[((v,a)) => v], reduceInMarking).asRep[((v, s)) => v]
      val sPackKey = sliceIn(packKey, sm).asRep[s => String]
      val sMapKey = sliceIn(mapKey, sm).asRep[s => k]
      val eK = mapKey.elem.eRange
      val eS = xs.elem.eRow
      assert(eS == sPackKey.elem.eDom,   s"${eS} != ${sPackKey.elem.eDom}")
      assert(eK == sMapKey.elem.eRange, s"${eK} != ${sMapKey.elem.eRange}")
      assert(eS == sMapKey.elem.eDom, s"${eS} == ${sMapKey.elem.eDom}")
//      assert(eS == newValue.elem.eItem, s"${eS} == ${newValue.elem.eItem}")
      xs.mapReduce(sMapKey, sPackKey, newValue, sReduce)

    case IterMethods.join(
            IsSliced(ls: RIter[s]@unchecked, IterMarking(All, mA: SliceMarking[a])),
            _rs, _lk, rk: RFunc[b,k]@unchecked, _cr) =>
      implicit val eA = mA.elem
      implicit val eB = rk.elem.eDom
      val rs = _rs.asRep[Iter[b]]
      val lk = _lk.asRep[a => k]
      val cr = _cr.asRep[b => b]
      val slk = sliceIn(lk, mA).asRep[s => k]
      val eS = ls.elem.eRow
      val eK = lk.elem.eRange
      assert(eS == slk.elem.eDom, s"$eS == ${slk.elem.eDom}")
      assert(eK == slk.elem.eRange, s"$eK == ${slk.elem.eRange}")
      assert(eK == rk.elem.eRange, s"$eK == ${rk.elem.eRange}")
      assert(mA.projectedElem == eS, s"${mA.projectedElem} == $eS")
      Sliced(ls.join(rs, slk, rk, cr), IterMarking(All, PairMarking(mA, eB.toMarking)))

    case IterMethods.join(_ls,
            IsSliced(rs: RIter[s]@unchecked, IterMarking(All, mB: SliceMarking[b])),
            lk: RFunc[a,k]@unchecked, _rk, _cr) =>
      implicit val eA = lk.elem.eDom
      implicit val eB = mB.elem
      val ls = _ls.asRep[Iter[a]]
      val rk = _rk.asRep[b => k]
      val cr = _cr.asRep[b => b]
      val srk = sliceIn(rk, mB).asRep[s => k]
      val scr = sliceFunc(cr, FuncMarking(mB, mB)).asRep[s => s]
      val eS = rs.elem.eRow
      val eK = rk.elem.eRange
      assert(eS == srk.elem.eDom, s"$eS == ${srk.elem.eDom}")
      assert(eK == srk.elem.eRange, s"$eK == ${srk.elem.eRange}")
      assert(eK == lk.elem.eRange, s"$eK == ${lk.elem.eRange}")
      assert(mB.projectedElem == eS, s"${mB.projectedElem} == $eS")
      Sliced(ls.join(rs, lk, srk, scr), IterMarking(All, PairMarking(eA.toMarking, mB)))

    case IterMethods.reduce(
            IsSliced(xs: RIter[s]@unchecked, IterMarking(KeyPath.All, mA: SliceMarking[a])),
            _f, init: Th[b] @unchecked) =>
      val f = _f.asRep[((b, a)) => b]
      implicit val eA = mA.elem
      implicit val eB = init.elem.eItem
      val mSlicedDom = PairMarking(eB.toMarking, mA)
      val sf = sliceIn(f, mSlicedDom).asRep[((b,s)) => b]
      val eS = xs.elem.eRow
      assert(eS == sf.elem.eDom.eSnd, s"$eS == ${sf.elem.eDom.eSnd}")
      assert(eB == sf.elem.eRange, s"$eB == ${sf.elem.eRange}")
      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
      xs.reduce(sf, init)

    case IterMethods.sortBy(
            IsSliced(xs: RIter[s]@unchecked, im @ IterMarking(All, mA: SliceMarking[a])), _c) =>
      val c = _c.asRep[((a,a)) => Int]
      val sp = sliceIn(c, PairMarking(mA,mA)).asRep[((s,s)) => Int]
      val eS = xs.elem.eRow
      assert(eS == sp.elem.eDom.eFst, s"$eS == ${sp.elem.eDom.eFst}")
      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
      Sliced(xs.sortBy(sp), im.asMark[Iter[a]])

    case IterMethods.sort(
            IsSliced(xs: RIter[s]@unchecked, im @ IterMarking(All, mA: SliceMarking[a])), _c) =>
      val c = _c.asRep[((a,a)) => Boolean]
      val sp = sliceIn(c, PairMarking(mA,mA)).asRep[((s,s)) => Boolean]
      val eS = xs.elem.eRow
      assert(eS == sp.elem.eDom.eFst, s"$eS == ${sp.elem.eDom.eFst}")
      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
      Sliced(xs.sort(sp), im.asMark[Iter[a]])

    case IterMethods.isEmpty(
            IsSliced(xs: RIter[s]@unchecked, IterMarking(KeyPath.All, mA: SliceMarking[a]))) =>
      val eA = mA.elem
      val eS = xs.elem.eRow
      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
      xs.isEmpty

    case UnpackSliced(IsSliced(x, m1), m2) if m1 == m2 =>
      x

    case IterMethods.map(iter, Def(IdentityLambda())) =>
      iter
    case IterMethods.map(ys @ Def(d2), f: Rep[Function1[a, b]]@unchecked) =>
      d2.asDef[Iter[a]] match {
        case IterMethods.map(xs: Rep[Iter[c]]@unchecked, g) => //TODO if hasSingleUsage(ys)
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

    case _ => super.rewriteDef(d)
  }}

}
