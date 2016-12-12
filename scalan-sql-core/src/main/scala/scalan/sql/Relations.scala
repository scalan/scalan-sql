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

  // Many (currently all) rules below have equivalents for iters, which would fire on the next stage,
  // but having them for relations as well allows better cost estimation.
  // If EmptyRelation, ConditionalRelation, etc. are added, uncomment the relevant rules and fix if necessary.
  // Same if new methods are added.
  // Alternative would be to lower to Iters _before_ evaluating cost.
  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {

//    case ExpConditionalRelation(Def(Const(b)), relation) =>
//      if (b)
//        relation
//      else
//        Relation.empty(relation.elem.eRow)
//
//    // must be last rule for ExpConditionalRelation
//    case ExpConditionalRelation(cond, relation @ Def(relationDef)) =>
//      relationDef match {
//        case _: EmptyRelation[_] =>
//          relation
//        case ExpConditionalRelation(cond1, relation1) =>
//          ExpConditionalRelation(cond && cond1, relation1)(relation1.elem.eRow)
//        case _ => super.rewriteDef(d)
//      }
//
//    case RelationMethods.isEmpty(Def(ExpConditionalRelation(condition, baseRelation))) =>
//      !condition || baseRelation.isEmpty
//    case RelationMethods.toArray(Def(ExpConditionalRelation(condition, baseRelation))) =>
//      IF (condition) THEN baseRelation.toArray ELSE SArray.empty(baseRelation.elem.eRow)
//    // has to be handled before other rules for RelationMethods.map/flatMap/etc. so they don't call super.rewriteDef
//    case MethodCall(condRelation @ Def(ExpConditionalRelation(condition, baseRelation)), m, args, neverInvoke) =>
//      val methodCall = mkMethodCall(baseRelation, m, args, neverInvoke)
//      methodCall match {
//        case baseRelation1: RRelation[a] @unchecked if baseRelation1.elem.isInstanceOf[RelationElem[_, _]] =>
//          conditionalRelation(condition, baseRelation1)
//        case nonRelation =>
//          !!!(s"(${baseRelation.toStringWithDefinition}).${m.getName} called inside ConditionalRelation, got non-relation ${nonRelation.toStringWithDefinition}", (condRelation +: args.collect { case e: Exp[_] => e }): _*)
//      }

    case RelationMethods.map(relation, Def(IdentityLambda())) =>
      relation
    case RelationMethods.map(ys @ Def(d2), f: RFunc[a, b] @unchecked) =>
      d2.asDef[Relation[a]] match {
        case RelationMethods.map(xs: RRelation[c] @unchecked, g) => //TODO if hasSingleUsage(ys)
          val g1 = g.asRep[c => a]
          implicit val eB = f.elem.eRange
          implicit val eC = xs.elem.asInstanceOf[RelationElem[c,_]].eRow
          val res = xs.map { x: Rep[c] => f(g1(x)) }
          res
        case RelationMethods.flatMap(xs: RRelation[c] @unchecked, Def(Lambda(l, _, x, Def(RelationMethods.map(ys: RRelation[d], g))))) =>
          val ys1 = ys.map(g.asRep[d => a] >> f)
          val f1 = copyLambda(l.asInstanceOf[Lambda[c, Relation[a]]], ys1)
          xs.flatMap(f1)
        case _ => super.rewriteDef(d)
      }
      case RelationMethods.filter(relation: RRelation[a], Def(VeryConstantLambda(b))) =>
        if (b) relation else super.rewriteDef(d) // emptyRelation(relation.elem.eRow)
//    case RelationMethods.filter(relation: RRelation[a], Def(ConstantLambda(c))) =>
//      conditionalRelation(c, relation)
    case RelationMethods.filter(relation @ Def(dRelation), f) =>
      dRelation match {
//        case RelationMethods.takeWhile(_, g) if f == g =>
//          relation
        case RelationMethods.filter(relation1: RRelation[a], g) =>
          relation1.filter(f.asRep[a => Boolean] &&& g.asRep[a => Boolean])
        case RelationMethods.flatMap(xs: RRelation[c] @unchecked, Def(Lambda(l, _, x, Def(RelationMethods.filter(ys: RRelation[a], g))))) =>
          val ys1 = ys.filter(f.asRep[a => Boolean] &&& g.asRep[a => Boolean])
          val f1 = copyLambda(l.asInstanceOf[Lambda[c, Relation[a]]], ys1)
          xs.flatMap(f1)
        case _ => super.rewriteDef(d)
      }
//    case RelationMethods.takeWhile(relation: RRelation[a], Def(ConstantLambda(c))) =>
//      conditionalRelation(c, relation)
//    case RelationMethods.takeWhile(relation @ Def(dRelation), f) =>
//      dRelation match {
//        case RelationMethods.filter(_, g) if f == g =>
//          relation
//        case RelationMethods.takeWhile(relation1: RRelation[a], g) =>
//          relation1.takeWhile(f.asRep[a => Boolean] &&& g.asRep[a => Boolean])
//        case _ => super.rewriteDef(d)
//      }

//    // must be last rule for flatMap
    case RelationMethods.flatMap(_relation, Def(Lambda(l: Lambda[a, Relation[b]] @unchecked, _, _, y @ Def(d1)))) =>
      val relation = _relation.asRep[Relation[a]]
      d1 match {
//        case _: EmptyRelation[_] =>
//          y // empty relation of the correct type
//        case ExpSingletonRelation(value) =>
//          val f1 = copyLambda(l, value)
//          relation.map(f1)
//        case ExpConditionalRelation(condition, Def(ExpSingletonRelation(value))) =>
//          val optValue = Pair(condition, value)
//          val f1 = copyLambda(l, optValue)
//          relation.flatMap0or1(f1)
        case RelationMethods.map(relation1: RRelation[c] @unchecked, g) if !l.scheduleSyms.contains(g) =>
          val f1 = copyLambda(l, relation1)
          relation.flatMap(f1).map(g.asRep[c => b])
        case RelationMethods.filter(relation1: RRelation[c] @unchecked, g) if !l.scheduleSyms.contains(g) =>
          val f1 = copyLambda(l, relation1)
          relation.flatMap(f1).filter(g.asRep[c => Boolean])
        case _ => super.rewriteDef(d)
      }
//
//    // last rule for flatMap0or1
//    case RelationMethods.flatMap0or1(_relation, f @ Def(Lambda(l: Lambda[a, Opt[b]] @unchecked, _, _, Def(y)))) =>
//      val relation = _relation.asRep[Relation[a]]
//      y match {
//        case Tup(Def(Const(b)), v) =>
//          if (b) {
//            val f1 = copyLambda(l, v)
//            relation.map(f1)
//          } else
//            Relation.empty(v.elem)
//        case _view: PairView[_, b1, _, _] =>
//          // TODO this rule works around PairView getting created in StructsPass, but creates extra map
//          // find if just disabling StructsPass works better, or there is other way to do it
//          val view = _view.asInstanceOf[PairView[Boolean, b1, Boolean, b]]
//          val iso1 = view.iso1
//          val iso2 = view.iso2
//          assert(iso1.isIdentity && iso1.eTo == BooleanElement)
//          val iso = view.iso
//          val f1 = iso.fromFun << f.asRep[a => Opt[b]]
//          val relation1 = relation.flatMap0or1(f1)
//          relation1.map(iso2.toFun)
//
//        // doesn't work (stack overflow)
//        //          implicit val eOptB = iso.eTo
//        //          val f1 = f.asRep[a => Opt[b]] >> fun[Opt[b], Opt[b]] { optB =>
//        //            val optB1 = iso.from(optB)
//        //            val newB = iso2.to(optB1._2)
//        //            (optB1._1, newB)
//        //          }
//        //          relation.flatMap0or1(f1)
//
//        // also doesn't work
//        //          implicit val eOptB1 = iso.eFrom
//        //          implicit val eOptB = iso.eTo
//        //          val f1 = f.asRep[a => Opt[b]] >> iso.fromFun >> fun { x: ROpt[b1] =>
//        //            (x._1, iso2.to(x._2))
//        //          }
//        //          relation.flatMap0or1(f1)
//
//        case _ => super.rewriteDef(d)
//      }
//
//    case TableRelationMethods.byRowids(relation, Def(ExpSingletonRelation(value: Rep[a])), f) =>
//      relation.uniqueByRowid(f.asRep[a => Rowid](value))
//    case TableRelationMethods.byRowids(relation, Def(ExpConditionalRelation(c, baseRelation: RRelation[a] @unchecked)), f) =>
//      conditionalRelation(c, relation.byRowids(baseRelation, f.asRep[a => Rowid]))

    case _ => super.rewriteDef(d)
  }

}
