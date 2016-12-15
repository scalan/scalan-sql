package scalan.sql

import java.lang.reflect.Method

import scalan._
import scala.reflect.runtime.universe._
import scalan.sql.parser.SqlAST._

trait Iters extends ScalanDsl {
  self: ItersDsl with ScalanSql =>

  type RIter[A] = Rep[Iter[A]]

  trait Iter[Row] extends Def[Iter[Row]] {
    def eRow: Elem[Row]

    def map[B](f: Rep[Row => B]): RIter[B] = delayInvoke

    // second parameter is used to create an initial value
    def mapU[B](f: Rep[((B, Row)) => Unit], eB: Elem[B]): RIter[B] = delayInvoke

    def flatMap[B](f: Rep[Row => Iter[B]]): RIter[B] = delayInvoke

    def flatMap0or1[B](f: Rep[Row => Opt[B]]): RIter[B] = delayInvoke

    def flatMap0or1U[B](f: Rep[((B, Row)) => Boolean]): RIter[B] = delayInvoke

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

    // when adding joinType make sure hasAtMostOneRow checks it's Inner
    def join[B, Key](other: RIter[B], thisKey: Rep[Row => Key], otherKey: Rep[B => Key], cloneOther: Rep[B => B]/*, joinType: JoinType*/): RIter[(Row, B)] = delayInvoke

    def toArray: Arr[Row] = delayInvoke

    // always called with Clone and equivalent to `map`, but semantically different
    def materialize(f: Rep[Row => Row]): RIter[Row] = delayInvoke
  }
  trait IterCompanion {
    def empty[Row: Elem]: Rep[EmptyIter[Row]] =
      EmptyIter()
    def single[Row](value: Rep[Row]): Rep[SingletonIter[Row]] =
      SingletonIter(value)(rep_getElem(value))
    def singleIf[Row](condition: Rep[Boolean], value: Rep[Row]): Rep[ConditionalIter[Row]] = {
      val elem = rep_getElem(value)
      val singleton = SingletonIter(value)(elem)
      ConditionalIter(condition, singleton)(elem)
    }
  }

  trait CursorIter[Row] extends Iter[Row] {
    def eRow: Elem[Row]

    def table: Rep[Table]
    def scanId: Rep[Int]
    def direction: Rep[SortDirection]
    def kernelInput: Rep[KernelInput]

    def fromBeginning(fakeDep: Rep[_]): RIter[Row] = delayInvoke
    def fromKeyWhile(keyValues: Rep[Array[Any]], operation: ComparisonOp, takeWhilePred: Rep[Row => Boolean], fakeDep: Rep[_]): RIter[Row] = delayInvoke

    // if there are cases where value is guaranteed to exist, add argument for this
    // I don't think there are any at the moment
    def uniqueByKey(keyValues: Rep[Array[Any]]): RIter[Row] = delayInvoke
    // Could be implemented as below but we don't want to invoke it at the top level, only inside a join/subquery.
    // Instead done explicitly by UniqueIterRewriter, search for its usage.
//    {
//      // note the call on self to prevent DelayInvokeException passing through
//      val optValue = self.asRep[CursorIter[Row]].uniqueValueByKey(keyValues)
//      Iter.singleIf(optValue._1, optValue._2)
//    }

    // TODO should advanceIter return Boolean as well, so this can be implemented?
    /** The first part of the result may not be accessed if the second is false */
    def uniqueValueByKey(keyValues: Rep[Array[Any]]): ROpt[Row] = delayInvoke
  }

  abstract class TableIter[Row](val table: Rep[Table], val scanId: Rep[Int], val direction: Rep[SortDirection], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends CursorIter[Row] {
    def byRowids[B](iter: RIter[B], f: Rep[B => Rowid]): RIter[Row] = delayInvoke

    // see comments for CursorIter#uniqueByKey
    def uniqueByRowid(rowid: Rep[Rowid]): RIter[Row] = delayInvoke

    // TODO as for uniqueValue above
    /** The first part of the result may not be accessed if the second is false */
    def uniqueValueByRowid(rowid: Rep[Rowid]): ROpt[Row] = delayInvoke

    def fromRowidWhile(rowid: Rep[Rowid], takeWhilePred: Rep[Row => Boolean], fakeDep: Rep[_]): RIter[Row] = delayInvoke
  }

  abstract class IndexIter[Row](val table: Rep[Table], val index: Rep[Index], val scanId: Rep[Int], val direction: Rep[SortDirection], val kernelInput: Rep[KernelInput])(implicit val eRow: Elem[Row]) extends CursorIter[Row]

  trait AtMostOne[Row] extends Iter[Row] {
    def eRow: Elem[Row]

    override def takeWhile(f: Rep[Row => Boolean]): RIter[Row] = filter(f)

    // TODO move up to Iter
    def reduceValue[B](f: Rep[((B, Row)) => B], init: Rep[Thunk[B]]): Rep[B]

    // def reduceValueU[B](f: Rep[((B, Row)) => Unit], init: Rep[Thunk[B]]): Rep[B] = delayInvoke

    override def reduce[B](f: Rep[((B, Row)) => B], init: Rep[Thunk[B]]): Rep[SingletonIter[B]] = {
      val v = reduceValue(f, init)
      Iter.single(v)
    }

    override def partialMapReduce[K, V](prefixComparator: Rep[((Row, Row)) => Boolean],
                               mapKey: Rep[Row => K],
                               packKey: Rep[Row => String],
                               newValue: Rep[Thunk[V]],
                               reduceValue: Rep[((V, Row)) => V]
                              ): RIter[Struct] = mapReduce(mapKey, packKey, newValue, reduceValue)

    override def sort(comparator: Rep[((Row, Row)) => Boolean]): RIter[Row] = self
    override def sortBy(comparator: Rep[((Row, Row)) => Int]): RIter[Row] = self

    override def partialSort(prefixComparator: Rep[((Row, Row)) => Boolean], suffixComparator: Rep[((Row, Row)) => Boolean]): RIter[Row] = self
  }

  abstract class SingletonIter[Row](val value: Rep[Row])(implicit val eRow: Elem[Row]) extends AtMostOne[Row] {
    override def map[B](f: Rep[Row => B]): RIter[B] =
      Iter.single(f(value))

    override def flatMap[B](f: Rep[Row => Iter[B]]): RIter[B] = f(value)

    override def flatMap0or1[B](f: Rep[Row => Opt[B]]): RIter[B] = {
      val optValue = f(value)
      Iter.singleIf(optValue._1, optValue._2)
    }

    override def filter(f: Rep[Row => Boolean]): RIter[Row] =
      Iter.singleIf(f(value), value)

    override def isEmpty: Rep[Boolean] = false

    override def reduceValue[B](f: Rep[((B, Row)) => B], init: Rep[Thunk[B]]): Rep[B] = {
      val initialValue = init.force()
      f((initialValue, value))
    }

    override def mapReduce[K, V](mapKey: Rep[Row => K],
                        packKey: Rep[Row => String],
                        newValue: Rep[Thunk[V]],
                        reduceValue: Rep[((V, Row)) => V]
                       ): RIter[Struct] = {
      val key = mapKey(value)
      val v = this.reduceValue(reduceValue, newValue)
      val result = struct(defaultStructTag, KeyFieldName -> key, ValueFieldName -> v)
      Iter.single(result)
    }

    // if `leftIsOuter` is true, `other` will be hashed; otherwise, `this` will be
    override def join[B, Key](other: RIter[B], thisKey: Rep[Row => Key], otherKey: Rep[B => Key], cloneOther: Rep[B => B]/*, joinType: JoinType*/): RIter[(Row, B)] = {
      val key = thisKey(value)
      val BtoKeyElem = rep_getElem(otherKey)
      implicit val eB = BtoKeyElem.eDom
      implicit val eKey = BtoKeyElem.eRange
      val f1 = fun { otherKey(_: Rep[B]) === key }
      val f2 = fun { Pair(value, _: Rep[B]) }
      other.filter(f1).map(f2)
    }

    override def toArray: Arr[Row] = SArray.singleton(value)
  }

  abstract class EmptyIter[Row](implicit val eRow: Elem[Row]) extends AtMostOne[Row] {
    override def map[B](f: Rep[Row => B]): RIter[B] = {
      val eB = rep_getElem(f).eRange
      EmptyIter()(eB)
    }

    override def flatMap[B](f: Rep[Row => Iter[B]]): RIter[B] = {
      val eB = rep_getElem(f).eRange.eRow
      EmptyIter()(eB)
    }

    override def flatMap0or1[B](f: Rep[Row => Opt[B]]): RIter[B] = {
      val eB = rep_getElem(f).eRange.eSnd
      EmptyIter()(eB)
    }

    override def filter(f: Rep[Row => Boolean]): RIter[Row] = self

    override def isEmpty: Rep[Boolean] = true

    override def reduceValue[B](f: Rep[((B, Row)) => B], init: Rep[Thunk[B]]): Rep[B] =
      init.force()

    override def mapReduce[K, V](mapKey: Rep[Row => K],
                                 packKey: Rep[Row => String],
                                 newValue: Rep[Thunk[V]],
                                 reduceValue: Rep[((V, Row)) => V]
                                ): RIter[Struct] = {
      val eK = rep_getElem(mapKey).eRange
      val eV = rep_getElem(newValue).eItem
      val resultElem = structElement(Seq(KeyFieldName -> eK, ValueFieldName -> eV))
      EmptyIter()(resultElem)
    }

    // if `leftIsOuter` is true, `other` will be hashed; otherwise, `this` will be
    override def join[B, Key](other: RIter[B], thisKey: Rep[Row => Key], otherKey: Rep[B => Key], cloneOther: Rep[B => B]/*, joinType: JoinType*/): RIter[(Row, B)] = {
      val BtoKeyElem = rep_getElem(otherKey)
      implicit val eB = BtoKeyElem.eDom
      Iter.empty
    }

    override def toArray: Arr[Row] = SArray.empty
  }

  // all methods "implemented" by rewriteDef
  // TODO check if just using IF THEN ELSE directly works as well
  /** Equivalent to IF (condition) THEN baseIter ELSE Iter.empty, may be rewritten to it in later stage */
  abstract class ConditionalIter[Row](val condition: Rep[Boolean], val baseIter: RIter[Row])(implicit val eRow: Elem[Row]) extends Iter[Row]
}

// TODO add rewrite rules map(IdentityLambda) etc.
trait ItersDsl extends impl.ItersAbs { self: ScalanSql =>
  type Opt[A] = (Boolean, A)
  type ROpt[A] = Rep[Opt[A]]

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

  def conditionalIter[Row](condition: Rep[Boolean], baseIter: Rep[Iter[Row]]) =
    ConditionalIter(condition, baseIter)(rep_getElem(baseIter).eRow)

  implicit def iterElemExtensions[A](ie: Elem[Iter[A]]): IterElem[A, Iter[A]] = ie.asInstanceOf[IterElem[A, Iter[A]]]

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
        case "filter" | "takeWhile" | "fromBeginning" | "fromKeyWhile" | "fromRowidWhile" | "sort" | "sortBy" | "materialize" | "seekIndex" | "partialSort" | "byRowids" | "uniqueByKey" | "uniqueByRowid" =>
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
        case "flatMap0or1" =>
          val f = args(0).asInstanceOf[Exp[_]]
          val eB = f.elem.asInstanceOf[FuncElem[_, _]].eRange.asInstanceOf[PairElem[_, _]].eSnd
          iterElement(eB)
        case "flatMap0or1U" =>
          val f = args(0).asInstanceOf[Exp[_]]
          val eB = f.elem.asInstanceOf[FuncElem[_, _]].eDom.asInstanceOf[PairElem[_, _]].eFst
          iterElement(eB)
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
        case "uniqueValueByKey" | "uniqueValueByRowid" =>
          pairElement(BooleanElement, iterElem.eRow)
        case _ => super.getResultElem(receiver, m, args)
      }
    case _ =>
      super.getResultElem(receiver, m, args)
  }

  def hasAtMostOneRow(dIterOrRelation: Def[_]): Boolean = dIterOrRelation match {
    case _: SingletonIter[_] | _: EmptyIter[_] =>
      true
    case IterMethods.reduce(_, _, _) =>
      true
    case MethodCall(_, m, _, _) if m.getName.startsWith("uniqueBy") =>
      true
    // TODO add case for unique Scannable#search (maybe it should be a separate method)
    case MethodCall(iterOrRelation, m, _, _) if {
      val name = m.getName
      name == "map" || name == "filter" || name == "takeWhile" || name == "mapReduce" ||
        name == "partialMapReduce"
    } =>
      hasAtMostOneRow(iterOrRelation)
    case ExpConditionalIter(_, iterOrRelation) =>
      hasAtMostOneRow(iterOrRelation)
    case IterMethods.join(iterOrRelation1, iterOrRelation2, _, _, _) =>
      hasAtMostOneRow(iterOrRelation1) && hasAtMostOneRow(iterOrRelation2)
    // this case could go into Relations.scala, but no point splitting like this
    case RelationMethods.hashJoin(iterOrRelation1, iterOrRelation2, _, _, _) =>
      hasAtMostOneRow(iterOrRelation1) && hasAtMostOneRow(iterOrRelation2)
    case _ => false
  }

  def hasAtMostOneRow(iterOrRelation: Exp[_]): Boolean = iterOrRelation match {
    case Def(dIterOrRelation) =>
      hasAtMostOneRow(dIterOrRelation)
    case _ => false
  }

  // hacky, but should be equivalent to building the lambda using `fun` as normal
  def copyLambda[A, B, C](l: Lambda[A, B], v: Rep[C]): Rep[A => C] = {
    val x = l.x
    implicit val eA = x.elem
    implicit val eC = v.elem
    val fSym = fresh[A => C]
    val l1 = new Lambda(None, x, v, fSym, l.mayInline)
    findOrCreateDefinition(l1, fSym)
  }

  override def rewriteDef[T](d: Def[T]): Exp[_] = (d: @unchecked) match {
    case ii @ ExpIndexIter(table, index, scanId, direction, kernelInput) if isIntegerPkIndex(index.asValue, table.asValue) =>
      TableIter(table, scanId, direction, kernelInput)(ii.eRow)

    case CursorIterMethods.uniqueByKey(tableIter @ Def(_: TableIter[a]), Def(SymsArray(Seq(value)))) =>
      tableIter.asRep[TableIter[a]].uniqueByRowid(value.asRep[Rowid])
    case CursorIterMethods.uniqueValueByKey(tableIter @ Def(_: TableIter[a]), Def(SymsArray(Seq(value)))) =>
      tableIter.asRep[TableIter[a]].uniqueValueByRowid(value.asRep[Rowid])
    case CursorIterMethods.fromKeyWhile(tableIter @ Def(_: TableIter[a]), Def(SymsArray(Seq(value))), op, f, fakeDep) =>
      val rowid0 = value.asRep[Rowid]
      val rowid = op match {
        case Eq | GreaterEq | LessEq | Is =>
          rowid0
        case Greater =>
          rowid0 + rowidNum.fromInt(1)
        case Less =>
          rowid0 - rowidNum.fromInt(1)
      }
      tableIter.asRep[TableIter[a]].fromRowidWhile(rowid, f.asRep[a => Boolean], fakeDep)

    case ExpConditionalIter(Def(Const(b)), iter) =>
      if (b)
        iter
      else
        Iter.empty(iter.elem.eRow)

    // must be last rule for ExpConditionalIter
    case ExpConditionalIter(cond, iter @ Def(iterDef)) =>
      iterDef match {
        case _: EmptyIter[_] =>
          iter
        case ExpConditionalIter(cond1, iter1) =>
          ExpConditionalIter(cond && cond1, iter1)(iter1.elem.eRow)
        case _ => super.rewriteDef(d)
      }

    case IterMethods.isEmpty(Def(ExpConditionalIter(condition, baseIter))) =>
      !condition || baseIter.isEmpty
    case IterMethods.toArray(Def(ExpConditionalIter(condition, baseIter))) =>
      IF (condition) THEN baseIter.toArray ELSE SArray.empty(baseIter.elem.eRow)
    // has to be handled before other rules for IterMethods.map/flatMap/etc. so they don't call super.rewriteDef
    case MethodCall(condIter @ Def(ExpConditionalIter(condition, baseIter)), m, args, neverInvoke) =>
      val methodCall = mkMethodCall(baseIter, m, args, neverInvoke)
      methodCall match {
        case baseIter1: RIter[a] @unchecked if baseIter1.elem.isInstanceOf[IterElem[_, _]] =>
          conditionalIter(condition, baseIter1)
        case nonIter =>
          !!!(s"(${baseIter.toStringWithDefinition}).${m.getName} called inside ConditionalIter, got non-iter ${nonIter.toStringWithDefinition}", (condIter +: args.collect { case e: Exp[_] => e }): _*)
      }

    case IterMethods.map(iter, Def(IdentityLambda())) =>
      iter
    case IterMethods.map(ys @ Def(d2), f: RFunc[a, b] @unchecked) =>
      d2.asDef[Iter[a]] match {
        case IterMethods.map(xs: RIter[c] @unchecked, g) => //TODO if hasSingleUsage(ys)
          val g1 = g.asRep[c => a]
          implicit val eB = f.elem.eRange
          implicit val eC = xs.elem.asInstanceOf[IterElem[c,_]].eRow
          val res = xs.map { x: Rep[c] => f(g1(x)) }
          res
        case IterMethods.flatMap(xs: RIter[c] @unchecked, Def(Lambda(l, _, x, Def(IterMethods.map(ys: RIter[d], g))))) =>
          val ys1 = ys.map(g.asRep[d => a] >> f)
          val f1 = copyLambda(l.asInstanceOf[Lambda[c, Iter[a]]], ys1)
          xs.flatMap(f1)
        case _ => super.rewriteDef(d)
      }
    case IterMethods.filter(iter: RIter[a], Def(ConstantLambda(c))) =>
      conditionalIter(c, iter)
    case IterMethods.filter(iter @ Def(dIter), f) =>
      dIter match {
        case IterMethods.takeWhile(_, g) if f == g =>
          iter
        case IterMethods.filter(iter1: RIter[a], g) =>
          iter1.filter(f.asRep[a => Boolean] &&& g.asRep[a => Boolean])
        case IterMethods.flatMap(xs: RIter[c] @unchecked, Def(Lambda(l, _, x, Def(IterMethods.filter(ys: RIter[a] @unchecked, g))))) =>
          val ys1 = ys.filter(f.asRep[a => Boolean] &&& g.asRep[a => Boolean])
          val f1 = copyLambda(l.asInstanceOf[Lambda[c, Iter[a]]], ys1)
          xs.flatMap(f1)
        case _ => super.rewriteDef(d)
      }
    case IterMethods.takeWhile(iter: RIter[a], Def(ConstantLambda(c))) =>
      conditionalIter(c, iter)
    case IterMethods.takeWhile(iter @ Def(dIter), f) =>
      dIter match {
        case IterMethods.filter(_, g) if f == g =>
          iter
        case IterMethods.takeWhile(iter1: RIter[a], g) =>
          iter1.takeWhile(f.asRep[a => Boolean] &&& g.asRep[a => Boolean])
        case _ => super.rewriteDef(d)
      }

    // must be last rule for flatMap
    case IterMethods.flatMap(_iter, Def(Lambda(l: Lambda[a, Iter[b]] @unchecked, _, _, y @ Def(d1)))) =>
      val iter = _iter.asRep[Iter[a]]
      d1 match {
        case _: EmptyIter[_] =>
          y // empty iter of the correct type
        case ExpSingletonIter(value) =>
          val f1 = copyLambda(l, value)
          iter.map(f1)
        case ExpConditionalIter(condition, Def(ExpSingletonIter(value))) =>
          val optValue = Pair(condition, value)
          val f1 = copyLambda(l, optValue)
          iter.flatMap0or1(f1)
        // below case should only happen in flatMap or in kernel's top level lambda
        // add similar rewrite rules if found elsewhere
        case _ if l.schedule.exists(te => UniqueIterRewriter.isDefinedAt(te.sym)) =>
          val f1 = rewriteLambda(l, UniqueIterRewriter)
          iter.flatMap(f1)
        // pull out map and filter from the inner loop if they don't depend on l's argument
        // this reduces overall number of calls and may give an opportunity for filter/filter
        // or map/map fusion
        case IterMethods.map(iter1: RIter[c] @unchecked, g) if !l.scheduleSyms.contains(g) =>
          val f1 = copyLambda(l, iter1)
          iter.flatMap(f1).map(g.asRep[c => b])
        case IterMethods.filter(iter1: RIter[c] @unchecked, g) if !l.scheduleSyms.contains(g) =>
          val f1 = copyLambda(l, iter1)
          iter.flatMap(f1).filter(g.asRep[c => Boolean])
        case _ => super.rewriteDef(d)
      }

    // last rule for flatMap0or1
    case IterMethods.flatMap0or1(_iter, f @ Def(Lambda(l: Lambda[a, Opt[b]] @unchecked, _, _, Def(y)))) =>
      val iter = _iter.asRep[Iter[a]]
      y match {
        case Tup(Def(Const(b)), v) =>
          if (b) {
            val f1 = copyLambda(l, v)
            iter.map(f1)
          } else
            Iter.empty(v.elem)
        case _view: PairView[_, b1, _, _] =>
          // TODO this rule works around PairView getting created in StructsPass, but creates extra map
          // find if just disabling StructsPass works better, or there is other way to do it
          val view = _view.asInstanceOf[PairView[Boolean, b1, Boolean, b]]
          val iso1 = view.iso1
          val iso2 = view.iso2
          assert(iso1.isIdentity && iso1.eTo == BooleanElement)
          val iso = view.iso
          val f1 = iso.fromFun << f.asRep[a => Opt[b]]
          val iter1 = iter.flatMap0or1(f1)
          iter1.map(iso2.toFun)

          // doesn't work (stack overflow)
//          implicit val eOptB = iso.eTo
//          val f1 = f.asRep[a => Opt[b]] >> fun[Opt[b], Opt[b]] { optB =>
//            val optB1 = iso.from(optB)
//            val newB = iso2.to(optB1._2)
//            (optB1._1, newB)
//          }
//          iter.flatMap0or1(f1)

          // also doesn't work
//          implicit val eOptB1 = iso.eFrom
//          implicit val eOptB = iso.eTo
//          val f1 = f.asRep[a => Opt[b]] >> iso.fromFun >> fun { x: ROpt[b1] =>
//            (x._1, iso2.to(x._2))
//          }
//          iter.flatMap0or1(f1)

        case _ => super.rewriteDef(d)
      }

    case MethodCall(receiver, m, _, _) if {
      val name = m.getName
      (name == "sort" || name == "sortBy" || name == "partialSort") &&
        hasAtMostOneRow(receiver)
    } =>
      receiver

    case TableIterMethods.byRowids(iter, Def(ExpSingletonIter(value: Rep[a])), f) =>
      iter.uniqueByRowid(f.asRep[a => Rowid](value))
    case TableIterMethods.byRowids(iter, Def(ExpConditionalIter(c, baseIter: RIter[a] @unchecked)), f) =>
      conditionalIter(c, iter.byRowids(baseIter, f.asRep[a => Rowid]))

    case CursorIterMethods.fromBeginning(cursorIter, fakeDep) if fakeDep == cursorIter.kernelInput =>
      // if fakeDep is kernelInput, we are in the top level function, and cursor is already at beginning when created
      cursorIter

    case _ => super.rewriteDef(d)
  }

  val UniqueIterRewriter: PartialFunction[Exp[_], Exp[_]] = {
    case Def(TableIterMethods.uniqueByRowid(iter, rowid)) =>
      val optValue = iter.uniqueValueByRowid(rowid)
      Iter.singleIf(optValue._1, optValue._2)
    case Def(CursorIterMethods.uniqueByKey(iter, key)) =>
      val optValue = iter.uniqueValueByKey(key)
      Iter.singleIf(optValue._1, optValue._2)
  }

  def rewriteLambda[A, B](lam: Lambda[A, B], rewriter: Rewriter): Exp[A => B] = {
    val newY = mirrorApplyRewriting(lam, lam.x, rewriter)
    copyLambda(lam, newY)
  }

  // TODO move up to Scalan after testing and implement mirrorApply using it
  def mirrorApplyRewriting[A,B](lam: Lambda[A, B], s: Exp[A], rewriter: Rewriter): Exp[B] = {
    val body = lam.scheduleSyms
    val (t, _) = DefaultMirror.mirrorSymbols(new MapTransformer(lam.x -> s), rewriter, lam, body)
    t(lam.y).asRep[B]
  }

}
