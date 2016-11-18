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

    /** Use instead of mapReduce when input is sorted by some prefix of the grouping.
      * @param prefixComparator returns true when two rows belong to the same group
      * @tparam K will be a pair of structs: group key and hash key (second may be empty)
      */
    def partialMapReduce[K, V](prefixComparator: Rep[((Row, Row)) => Boolean],
                               mapKey: Rep[Row => K],
                               newValue: Rep[Thunk[V]],
                               reduceValue: Rep[((V, Row)) => V]
                              ): RRelation[Struct] = delayInvoke

    // sort takes a less-than predicate, sortBy takes an Ordering.compare-like function
    def sort(comparator: Rep[((Row, Row)) => Boolean]): RRelation[Row] = delayInvoke
    def sortBy(comparator: Rep[((Row, Row)) => Int]): RRelation[Row] = delayInvoke

    /**
      * Use when input is already sorted on some prefix of desired ordering
      * @param prefixComparator return true when both rows belong to the same group
      * @param suffixComparator compare rows within one group, return true if the first is less than the second
      */
    def partialSort(prefixComparator: Rep[((Row, Row)) => Boolean], suffixComparator: Rep[((Row, Row)) => Boolean]): RRelation[Row] =
      delayInvoke

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

    override def partialMapReduce[K, V](prefixComparator: Rep[((Row, Row)) => Boolean],
                                        mapKey: Rep[Row => K],
                                        newValue: Rep[Thunk[V]],
                                        reduceValue: Rep[((V, Row)) => V]
                                       ): RRelation[Struct] = {
      val packKey: Rep[(Row) => String] =
        fun[Row, String] { x => pack(mapKey(x).asRep[(Struct, Struct)]._2) }
      val iter1 = iter.partialMapReduce(prefixComparator, mapKey, packKey, newValue, reduceValue)
      iterBasedRelation(iter1)
    }

    // sort takes a less-than predicate, sortBy takes an Ordering.compare-like function
    override def sort(comparator: Rep[((Row, Row)) => Boolean]): RRelation[Row] =
      IterBasedRelation(iter.materialize(cloneFun(eRow)).sort(comparator))
    override def sortBy(comparator: Rep[((Row, Row)) => Int]): RRelation[Row] =
      IterBasedRelation(iter.materialize(cloneFun(eRow)).sortBy(comparator))

    override def partialSort(prefixComparator: Rep[((Row, Row)) => Boolean], suffixComparator: Rep[((Row, Row)) => Boolean]) =
      IterBasedRelation(iter.materialize(cloneFun(eRow)).partialSort(prefixComparator, suffixComparator))

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

  def wrapStructRelation[A](env: Rep[Struct], f: RFunc[Struct, Iter[A]]): Rep[WrapStructRelation[A, Struct, Struct]] =
    ???
}

trait RelationsDslStd extends impl.RelationsStd { self: ScalanSqlStd =>
}

trait RelationsDslExp extends impl.RelationsExp { self: ScalanSqlExp =>
  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
    case relationElem: RelationElem[_, _] =>
      m.getName match {
        case "filter" | "sort" | "sortBy" | "partialSort" =>
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
        case "partialMapReduce" =>
          val mapKey = args(1).asInstanceOf[Exp[_]]
          val newValue = args(2).asInstanceOf[Exp[_]]
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

}
