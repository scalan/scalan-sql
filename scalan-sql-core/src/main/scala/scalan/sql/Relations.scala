package scalan.sql

import java.lang.reflect.Method

import scalan._
import scalan.common.Lazy
import scala.reflect.runtime.universe._
import scalan.sql.parser.SqlAST.{Index, Table}

trait Relations extends ScalanDsl {
  self: RelationsDsl with ScalanSql =>

  type RRelation[A] = Rep[Relation[A]]

  trait Relation[Row] extends Def[Relation[Row]] {
    def eRow: Elem[Row]

    def iter: RIter[Row]

    def map[B](f: Rep[Row => B]): RRelation[B] = delayInvoke

    def flatMap[B](f: Rep[Row => Relation[B]]): RRelation[B] = delayInvoke

    def filter(f: Rep[Row => Boolean]): RRelation[Row] = delayInvoke

    def isEmpty: Rep[Boolean] = delayInvoke

    def reduce[B](f: Rep[((B, Row)) => B], init: Rep[Thunk[B]]): RRelation[B] = delayInvoke

    /**
      * @return Contains structures of type {key: K, val: V}
      */
    def mapReduce[K, V](mapKey: Rep[Row => K],
                        newValue: Rep[Thunk[V]],
                        reduceValue: Rep[((V, Row)) => V]
                       ): RRelation[Struct] = delayInvoke

    // sort takes a less-than predicate, sortBy takes an Ordering.compare-like function
    def sort(comparator: Rep[((Row, Row)) => Boolean]): RRelation[Row] = delayInvoke
    def sortBy(comparator: Rep[((Row, Row)) => Int]): RRelation[Row] = delayInvoke

    def hashJoin[B, Key](other: RRelation[B], thisKey: Rep[Row => Key], otherKey: Rep[B => Key], leftIsOuter: Boolean /*, joinType: JoinType*/): RRelation[(Row, B)] = delayInvoke

    def onlyValue(): Rep[Row] = delayInvoke
  }

  // TODO this is either temporary or internal detail
  // Semantically there is one relation per table, but it can have many Iters.
  abstract class IterBasedRelation[Row](val iter: RIter[Row])(implicit val eRow: Elem[Row]) extends Relation[Row] {
    override def map[B](f: Rep[Row => B]): RRelation[B] = iterBasedRelation(iter.map(f))

    override def flatMap[B](f: Rep[Row => Relation[B]]): RRelation[B] = {
      val f1 = inferredFun(eRow) { x => f(x).iter }
      val iter1 = iter.flatMap(f1)
      implicit val eB = iter1.selfType1.eRow
      IterBasedRelation(iter1)
    }

    override def filter(f: Rep[Row => Boolean]): RRelation[Row] = IterBasedRelation(iter.filter(f))

    override def isEmpty: Rep[Boolean] = iter.isEmpty

    // TODO implement using reduceU
    override def reduce[B](f: Rep[((B, Row)) => B], init: Rep[Thunk[B]]): RRelation[B] =
      iterBasedRelation(iter.reduce(f, init))

    // TODO implement using mapReduceU
    /**
      * @return Contains structures of type {key: K, val: V}
      */
    override def mapReduce[K, V](mapKey: Rep[Row => K],
                        newValue: Rep[Thunk[V]],
                        reduceValue: Rep[((V, Row)) => V]
                       ): RRelation[Struct] = {
      val iter1 = iter.mapReduce(mapKey, fun[Row, String] { x => pack(mapKey(x)) }, newValue, reduceValue)
      iterBasedRelation(iter1)
    }

    // sort takes a less-than predicate, sortBy takes an Ordering.compare-like function
    override def sort(comparator: Rep[((Row, Row)) => Boolean]): RRelation[Row] =
      IterBasedRelation(iter.materialize(cloneFun(eRow)).sort(comparator))
    override def sortBy(comparator: Rep[((Row, Row)) => Int]): RRelation[Row] =
      IterBasedRelation(iter.materialize(cloneFun(eRow)).sortBy(comparator))

    // if `leftIsOuter` is true, `other` will be hashed; otherwise, `this` will be
    override def hashJoin[B, Key](other: RRelation[B], thisKey: Rep[Row => Key], otherKey: Rep[B => Key], leftIsOuter: Boolean /*, joinType: JoinType*/): RRelation[(Row, B)] = {
      implicit val eB = other.selfType1.eRow
      val iter1 = if (leftIsOuter)
        iter.join(other.iter, thisKey, otherKey, cloneFun(eB))
      else
        other.iter.join(iter, otherKey, thisKey, cloneFun(eRow)).map(fun {
          pair => (pair._2, pair._1)
        })

      iterBasedRelation(iter1)
    }

    override def onlyValue(): Rep[Row] =
      advanceIter(iter)._1
  }

  abstract class PhysicalRelation[Row](val source: RScannable[Row])(implicit val eRow: Elem[Row]) extends Relation[Row] {
    override def iter = delayInvoke
  }

  abstract class WrapRelation[Row, Row2](val env: RRelation[Row2], val f: RFunc[Iter[Row2], Iter[Row]])(
    implicit val eRow: Elem[Row], val eRow2: Elem[Row2]) extends Relation[Row] {
    override def iter = f(env.iter)
  }

  // Can't currently be implemented using just Struct because incorrect code (with element[Struct]) is generated
  // env struct contains relations, input to f contains iters with the same field names
  abstract class WrapStructRelation[Row, EnvR <: Struct, EnvI <: Struct](val env: Rep[EnvR], val f: RFunc[EnvI, Iter[Row]])(
    implicit val eRow: Elem[Row], eEnvR: Elem[EnvR], eEnvI: Elem[EnvI]) extends Relation[Row] {
    override def iter = {
      val itersEnv = env.mapFields {
        case rel: RRelation[a] @unchecked => rel.iter
      }.asRep[EnvI]
      f(itersEnv)
    }
  }
}

// TODO add rewrite rules map(IdentityLambda) etc.
trait RelationsDsl extends impl.RelationsAbs { self: ScalanSql =>

//  trait RelationFunctor extends Functor[Relation] {
//    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[Relation[T]]
//    def lift[T](implicit eT: Elem[T]) = element[Relation[T]]
//    def unlift[T](implicit eFT: Elem[Relation[T]]) = eFT.asInstanceOf[RelationElem[T,_]].eRow
//    def getElem[T](fa: Rep[Relation[T]]) = rep_getElem(fa)
//    def unapply[A](e: Elem[_]) = e match {
//      case ae: RelationElem[_, _] => Some(ae.asElem[Relation[A]])
//      case _ => None
//    }
//    def map[A:Elem,B:Elem](xs: Rep[Relation[A]])(f: Rep[A] => Rep[B]) = xs.map(fun(f))
//  }
//  implicit val RelationContainer: Functor[Relation] = new RelationFunctor {}
  implicit def RelationElemExtensions[A](ie: Elem[Relation[A]]) = ie.asInstanceOf[RelationElem[A, Relation[A]]]

  def iterBasedRelation[A](iter: RIter[A]) = IterBasedRelation(iter)(iter.selfType1.eRow)

  def physicalRelation[A](source: RScannable[A]) = PhysicalRelation(source)(source.selfType1.eRow)

  def wrapStructRelation[A](env: Rep[Struct], f: RFunc[Struct, Iter[A]]): Rep[WrapStructRelation[A, Struct, Struct]] =
    ???
}

trait RelationsDslStd extends impl.RelationsStd { self: ScalanSqlStd =>
}

trait RelationsDslExp extends impl.RelationsExp { self: ScalanSqlExp =>
//  case class RelationMarking[T](itemsPath: KeyPath, override val innerMark: SliceMarking[T]) extends SliceMarking1[T,Relation](innerMark) {
//    implicit val eItem = innerMark.elem
//    val elem = element[Relation[T]]
//    def children: Seq[SliceMarking[_]] = Seq(innerMark)
//    def meet(other: SliceMarking[Relation[T]]) = ???
//    def join(other: SliceMarking[Relation[T]]) = other match {
//      case am: RelationMarking[T] @unchecked if am.itemsPath == itemsPath =>
//        RelationMarking(itemsPath, innerMark.join(am.innerMark))
//      case am: RelationMarking[T] @unchecked if am.itemsPath == KeyPath.None =>
//        this
//      case am: RelationMarking[T] @unchecked if this.itemsPath == KeyPath.None =>
//        other
//    }
//    def >>[R](other: SliceMarking[R]): SliceMarking[Relation[T]] = other match {
//      case am: RelationMarking[_] if am.itemsPath == itemsPath =>
//        RelationMarking(itemsPath, innerMark >> am.innerMark)
//      case am: RelationMarking[_] if am.itemsPath == KeyPath.None =>
//        RelationMarking(KeyPath.None, innerMark >> am.innerMark)
//    }
//    def nonEmpty = innerMark.nonEmpty && (!itemsPath.isNone)
//    def isIdentity = itemsPath.isAll && innerMark.isIdentity
//    def |/|[R](key: KeyPath, inner: SliceMarking[R]) = key match {
//      case KeyPath.All if inner.elem == eItem =>
//        RelationMarking[T](key, inner.asMark[T])
//    }
//    def projectToExp(xs: Exp[Relation[T]]): Exp[_] = itemsPath match {
//      case KeyPath.All =>
//        assert(xs.elem == this.elem)
//        reifyObject(UnpackSliced(xs, this))
//      case KeyPath.None =>
//        Relation.empty[T]
//      case _ =>
//        !!!(s"itemsPath = $itemsPath")
//    }
//    val projectedElem = relationElement(innerMark.projectedElem)
//    def makeSlot = {
//      SlicedRelation(fresh(Lazy(projectedElem)), innerMark)
//    }
//    def set(slot: Exp[Relation[T]], value: Exp[_]) = slot match {
//      case Def(Relation: SlicedRelation[T,a]@unchecked) =>
//        SlicedRelation(value.asRep[Relation[a]], Relation.innerMark)
//      case _ =>
//        setInvalid(slot, value)
//    }
//  }
//
  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
    case relationElem: RelationElem[_, _] =>
      m.getName match {
        case "filter" | "sort" | "sortBy" =>
          receiver.elem
        case "map" =>
          val f = args(0).asInstanceOf[Exp[_]]
          val eB = f.elem.asInstanceOf[FuncElem[_, _]].eRange
          relationElement(eB)
        case "flatMap" =>
          val f = args(0).asInstanceOf[Exp[_]]
          val eB = f.elem.asInstanceOf[FuncElem[_, _]].eRange.asInstanceOf[RelationElem[_, _]].eRow
          relationElement(eB)
        case "mapReduce" =>
          val mapKey = args(0).asInstanceOf[Exp[_]]
          val newValue = args(1).asInstanceOf[Exp[_]]
          val eK = mapKey.elem.asInstanceOf[FuncElem[_, _]].eRange
          val eV = newValue.elem.asInstanceOf[ThunkElem[_]].eItem
          relationElement(keyValElem(eK, eV))
        case "hashJoin" =>
          val other = args(0).asInstanceOf[Exp[_]]
          val eB = other.elem.asInstanceOf[RelationElem[_, _]].eRow
          val eA = relationElem.eRow
          relationElement(pairElement(eA, eB))
        case "onlyValue" =>
          relationElem.eRow
        case "iter" =>
          iterElement(relationElem.eRow)
        case _ => super.getResultElem(receiver, m, args)
      }
    case _ =>
      super.getResultElem(receiver, m, args)
  }

  override def wrapStructRelation[A](env: Rep[Struct], f: RFunc[Struct, Iter[A]]) = {
    val eEnvR = env.elem
    val eF = f.elem
    val eEnvI = eF.eDom
    val eRow = eF.eRange.eRow
    WrapStructRelation[A, Struct, Struct](env, f)(eRow, eEnvR, eEnvI)
  }
//
//  override def rewriteDef[T](d: Def[T]): Exp[_] = { import KeyPath._; d match {
//    case RelationMethods.filter(
//            IsSliced(xs: RRelation[s]@unchecked, im @ RelationMarking(All, mA: SliceMarking[a])), _p) =>
//      val p = _p.asRep[a => Boolean]
//      val sp = sliceIn(p, mA).asRep[s => Boolean]
//      val eS = xs.elem.eRow
//      assert(eS == sp.elem.eDom, s"${eS} == ${sp.elem.eDom}")
//      Sliced(xs.filter(sp), im.asMark[Relation[a]])
//
//    case RelationMethods.map(
//            IsSliced(_xs, RelationMarking(All, mA: SliceMarking[a])),
//            _f: RFunc[_,b]@unchecked) =>
//      val f = _f.asRep[a => b]
//      val fm @ FuncMarking(_, mB) = sliceAnalyzer.getMark(f)
//      fm.projectedElem match {
//        case fe: FuncElem[s,t] =>
//          val xs = _xs.asRep[Relation[s]]
//          val fs = sliceFunc(f, FuncMarking(mA, mB.asMark[b])).asRep[s => t]
//          assert(xs.elem.eRow == fs.elem.eDom, s"${xs.elem.eRow} != ${fs.elem.eDom}")
//          assert(xs.elem.eRow == mA.projectedElem, s"${xs.elem.eRow} == ${mA.projectedElem}")
//          assert(fs.elem.eRange == mB.projectedElem, s"${fs.elem.eRange} == ${mB.projectedElem}")
//          Sliced(xs.map(fs), RelationMarking(All, mB))
//      }
//
//    case RelationMethods.mapReduce(
//            IsSliced(xs: RRelation[s]@unchecked, RelationMarking(KeyPath.All, sm: SliceMarking[a])),
//            _mapKey: RFunc[_,k]@unchecked, _packKey, newValue: Th[v] @unchecked, _reduceValue) =>
//      val mapKey = _mapKey.asRep[a => k]
//      val packKey = _packKey.asRep[a => String]
//      val eV = newValue.elem.eItem
//      val reduceInMarking = PairMarking(eV.toMarking, sm)
//      val sReduce = sliceIn(_reduceValue.asRep[((v,a)) => v], reduceInMarking).asRep[((v, s)) => v]
//      val sPackKey = sliceIn(packKey, sm).asRep[s => String]
//      val sMapKey = sliceIn(mapKey, sm).asRep[s => k]
//      val eK = mapKey.elem.eRange
//      val eS = xs.elem.eRow
//      assert(eS == sPackKey.elem.eDom,   s"${eS} != ${sPackKey.elem.eDom}")
//      assert(eK == sMapKey.elem.eRange, s"${eK} != ${sMapKey.elem.eRange}")
//      assert(eS == sMapKey.elem.eDom, s"${eS} == ${sMapKey.elem.eDom}")
////      assert(eS == newValue.elem.eItem, s"${eS} == ${newValue.elem.eItem}")
//      xs.mapReduce(sMapKey, sPackKey, newValue, sReduce)
//
//    case RelationMethods.join(
//            IsSliced(ls: RRelation[s]@unchecked, RelationMarking(All, mA: SliceMarking[a])),
//            _rs, _lk, rk: RFunc[b,k]@unchecked, _cr) =>
//      implicit val eA = mA.elem
//      implicit val eB = rk.elem.eDom
//      val rs = _rs.asRep[Relation[b]]
//      val lk = _lk.asRep[a => k]
//      val cr = _cr.asRep[b => b]
//      val slk = sliceIn(lk, mA).asRep[s => k]
//      val eS = ls.elem.eRow
//      val eK = lk.elem.eRange
//      assert(eS == slk.elem.eDom, s"$eS == ${slk.elem.eDom}")
//      assert(eK == slk.elem.eRange, s"$eK == ${slk.elem.eRange}")
//      assert(eK == rk.elem.eRange, s"$eK == ${rk.elem.eRange}")
//      assert(mA.projectedElem == eS, s"${mA.projectedElem} == $eS")
//      Sliced(ls.join(rs, slk, rk, cr), RelationMarking(All, PairMarking(mA, eB.toMarking)))
//
//    case RelationMethods.join(_ls,
//            IsSliced(rs: RRelation[s]@unchecked, RelationMarking(All, mB: SliceMarking[b])),
//            lk: RFunc[a,k]@unchecked, _rk, _cr) =>
//      implicit val eA = lk.elem.eDom
//      implicit val eB = mB.elem
//      val ls = _ls.asRep[Relation[a]]
//      val rk = _rk.asRep[b => k]
//      val cr = _cr.asRep[b => b]
//      val srk = sliceIn(rk, mB).asRep[s => k]
//      val scr = sliceFunc(cr, FuncMarking(mB, mB)).asRep[s => s]
//      val eS = rs.elem.eRow
//      val eK = rk.elem.eRange
//      assert(eS == srk.elem.eDom, s"$eS == ${srk.elem.eDom}")
//      assert(eK == srk.elem.eRange, s"$eK == ${srk.elem.eRange}")
//      assert(eK == lk.elem.eRange, s"$eK == ${lk.elem.eRange}")
//      assert(mB.projectedElem == eS, s"${mB.projectedElem} == $eS")
//      Sliced(ls.join(rs, lk, srk, scr), RelationMarking(All, PairMarking(eA.toMarking, mB)))
//
//    case RelationMethods.reduce(
//            IsSliced(xs: RRelation[s]@unchecked, RelationMarking(KeyPath.All, mA: SliceMarking[a])),
//            _f, init: Th[b] @unchecked) =>
//      val f = _f.asRep[((b, a)) => b]
//      implicit val eA = mA.elem
//      implicit val eB = init.elem.eItem
//      val mSlicedDom = PairMarking(eB.toMarking, mA)
//      val sf = sliceIn(f, mSlicedDom).asRep[((b,s)) => b]
//      val eS = xs.elem.eRow
//      assert(eS == sf.elem.eDom.eSnd, s"$eS == ${sf.elem.eDom.eSnd}")
//      assert(eB == sf.elem.eRange, s"$eB == ${sf.elem.eRange}")
//      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
//      xs.reduce(sf, init)
//
//    case RelationMethods.sortBy(
//            IsSliced(xs: RRelation[s]@unchecked, im @ RelationMarking(All, mA: SliceMarking[a])), _c) =>
//      val c = _c.asRep[((a,a)) => Int]
//      val sp = sliceIn(c, PairMarking(mA,mA)).asRep[((s,s)) => Int]
//      val eS = xs.elem.eRow
//      assert(eS == sp.elem.eDom.eFst, s"$eS == ${sp.elem.eDom.eFst}")
//      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
//      Sliced(xs.sortBy(sp), im.asMark[Relation[a]])
//
//    case RelationMethods.sort(
//            IsSliced(xs: RRelation[s]@unchecked, im @ RelationMarking(All, mA: SliceMarking[a])), _c) =>
//      val c = _c.asRep[((a,a)) => Boolean]
//      val sp = sliceIn(c, PairMarking(mA,mA)).asRep[((s,s)) => Boolean]
//      val eS = xs.elem.eRow
//      assert(eS == sp.elem.eDom.eFst, s"$eS == ${sp.elem.eDom.eFst}")
//      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
//      Sliced(xs.sort(sp), im.asMark[Relation[a]])
//
//    case RelationMethods.isEmpty(
//            IsSliced(xs: RRelation[s]@unchecked, RelationMarking(KeyPath.All, mA: SliceMarking[a]))) =>
//      val eA = mA.elem
//      val eS = xs.elem.eRow
//      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
//      xs.isEmpty
//
//    case UnpackSliced(IsSliced(x, m1), m2) if m1 == m2 =>
//      x
//
//    case RelationMethods.map(Relation, Def(IdentityLambda())) =>
//      Relation
//    case RelationMethods.map(ys @ Def(d2), f: Rep[Function1[a, b]]@unchecked) =>
//      d2.asDef[Relation[a]] match {
//        case RelationMethods.map(xs: Rep[Relation[c]]@unchecked, g) => //TODO if hasSingleUsage(ys)
//          val g1 = g.asRep[c => a]
//          implicit val eB = f.elem.eRange
//          implicit val eC = xs.elem.asInstanceOf[RelationElem[c,_]].eRow
//          val res = xs.map { x: Rep[c] => f(g1(x)) }
//          res
//        case _ => super.rewriteDef(d)
//      }
//
//    case _ => super.rewriteDef(d)
//  }}

}
