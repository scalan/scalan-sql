package scalan.sql

import scalan.staged.Slicing

trait SqlSlicing extends Slicing { ctx: ScalanSqlExp =>
  import KeyPath._

  val IterMarking = new TraversableMarkingFor[Iter]

  protected def createSliceAnalyzer: SliceAnalyzer = new SqlSliceAnalyzer
  
  class SqlSliceAnalyzer extends SliceAnalyzer {

    override def getInboundMarkings[T](te: TableEntry[T], outMark: SliceMarking[T]): MarkedSyms = {
      val thisSym = te.sym
      val d = te.rhs

      def filterLike[A](xs: Rep[Iter[A]], f: Rep[A => Boolean]) = outMark match {
        case IterMarking(_, inner: SliceMarking[A] @unchecked) =>
          val mOutMark = inner.asMark[A]
          val mRange = element[Boolean].toMarking
          val mf = analyzeFunc(f, mRange)
          val mDom = mf.mDom.join(mOutMark)
          val mxs = getMark(xs) |/| (All, mDom)
          Seq[MarkedSym]((xs, mxs), (thisSym.asRep[Iter[A]], mxs), f.marked(FuncMarking(mDom, mRange)))
      }

      def sortLike[A, B](xs: Rep[Iter[A]], fs: Seq[Rep[((A, A)) => B]]) = outMark match {
        case IterMarking(_, mA: SliceMarking[A] @unchecked) =>
          val mRange = fs.head.elem.eRange.toMarking
          val mDom = fs.foldLeft(mA) { (acc, f) =>
            val fm = analyzeFunc(f, mRange)
            val PairMarking(mD, _) = fm.mDom
            acc.join(mD)
          }
          val mFuncFinal = FuncMarking(PairMarking(mDom, mDom), mRange)
          val mxs = getMark(xs) |/| (All, mDom)
          Seq[MarkedSym]((xs, mxs), (thisSym.asRep[Iter[A]], mxs)) ++ fs.map(_.marked(mFuncFinal))
      }

      def uniqueLike[A, B](xs: Rep[Iter[A]], key: Rep[B]) =
        Seq[MarkedSym](xs.marked(outMark.asMark[Iter[A]]), key.marked(key.elem.toMarking))

      def uniqueValueLike[A, B](xs: Rep[Iter[A]], key: Rep[B]) = outMark match {
        case PairMarking(_, markA: SliceMarking[A] @unchecked) =>
          Seq[MarkedSym](xs.marked(IterMarking(All, markA)), key.marked(key.elem.toMarking))
      }

      d match {
        case IterMethods.map(_xs, f: RFunc[a, b] @unchecked) => outMark match {
          case IterMarking(_, mB_out) =>
            val xs = _xs.asRep[Iter[a]]
            val mOutMark = mB_out.asMark[b]
            val lm = analyzeFunc(f, mOutMark)
            Seq((xs, getMark(xs) |/| (All, lm.mDom)))
        }

        case IterMethods.flatMap(_xs, f: RFunc[a, Iter[b]] @unchecked) => outMark match {
          case IterMarking(_, mB_out) =>
            val xs = _xs.asRep[Iter[a]]
            val mOutMark = IterMarking(All, mB_out.asMark[b])
            val lm = analyzeFunc(f, mOutMark)
            Seq((xs, getMark(xs) |/| (All, lm.mDom)))
        }

        case IterMethods.flatMap0or1(_xs, f: RFunc[a, Opt[b]] @unchecked) => outMark match {
          case IterMarking(_, mB) =>
            val xs = _xs.asRep[Iter[a]]
            val mOutMark = PairMarking(BooleanElement.toMarking, mB)
            val lm = analyzeFunc(f, mOutMark)
            Seq((xs, getMark(xs) |/| (All, lm.mDom)))
        }

        case IterMethods.sortBy(xs: RIter[a] @unchecked, f) =>
          sortLike(xs, Seq(f.asRep[((a, a)) => Int]))

        case IterMethods.sort(xs: RIter[a] @unchecked, f) =>
          sortLike(xs, Seq(f.asRep[((a, a)) => Boolean]))

        case IterMethods.partialSort(xs: RIter[a] @unchecked, f1, f2) =>
          sortLike(xs, Seq(f1.asRep[((a, a)) => Boolean], f2.asRep[((a, a)) => Boolean]))

        case IterMethods.filter(xs: RIter[a], f) =>
          filterLike(xs, f.asRep[a => Boolean])

        case IterMethods.takeWhile(xs: RIter[a], f) =>
          filterLike(xs, f.asRep[a => Boolean])

        case CursorIterMethods.fromKeyWhile(xs: RIter[a] @unchecked, _, _, f, _) =>
          filterLike(xs, f.asRep[a => Boolean])

        case CursorIterMethods.fromBeginning(xs: RIter[a] @unchecked, _) =>
          Seq[MarkedSym](xs.marked(outMark.asMark[Iter[a]]))

        case IterMethods.mapReduce(_xs: RIter[a] @unchecked, _mapKey: RFunc[_, k], _packKey, _newValue: Th[v] @unchecked, _reduce) => outMark match {
          case IterMarking(_, mKeyVal: StructMarking[_]) =>
            val xs = _xs.asRep[a]
            val mapKey = _mapKey.asRep[a => k]
            val reduce = _reduce.asRep[((v, a)) => v]
            val pack = _packKey.asRep[k => String]
            val newValue = _newValue.asRep[Thunk[v]]
            implicit val eA = mapKey.elem.eDom
            implicit val eK = mapKey.elem.eRange
            implicit val eV = newValue.elem.eItem

            val mVal = mKeyVal.get(ValueFieldName).getOrElse(EmptyMarking(eV)).asMark[v]
            val FuncMarking(PairMarking(mV1, mA1), mV3) = analyzeFunc(reduce, mVal)
            val mVal1 = mVal.join(mV1.asMark[v]).join(mV3.asMark[v])
            val ThunkMarking(mVal2) = analyzeThunk(newValue, mVal1)

            val mKey = mKeyVal.get(KeyFieldName).getOrElse(EmptyMarking(eK)).asMark[k]
            val FuncMarking(mA2, _) = analyzeFunc(mapKey, mKey)
            val mPack = analyzeFunc(pack, StringElement.toMarking)
            assert(mPack.mDom == mA2)
            Seq((xs, xs.elem.toMarking |/| (All, mA1.asMark[a].join(mA2.asMark[a]))))
        }

        case IterMethods.partialMapReduce(_xs: RIter[a] @unchecked, _eq, _mapKey: RFunc[_, k], _packKey, _newValue: Th[v] @unchecked, _reduce) => outMark match {
          case IterMarking(_, mKeyVal: StructMarking[_]) =>
            val xs = _xs.asRep[a]
            val eq = _eq.asRep[((a, a)) => Boolean]
            val mapKey = _mapKey.asRep[a => k]
            val reduce = _reduce.asRep[((v, a)) => v]
            val pack = _packKey.asRep[k => String]
            val newValue = _newValue.asRep[Thunk[v]]
            implicit val eA = mapKey.elem.eDom
            implicit val eK = mapKey.elem.eRange
            implicit val eV = newValue.elem.eItem

            val mVal = mKeyVal.get(ValueFieldName).getOrElse(EmptyMarking(eV)).asMark[v]
            val FuncMarking(PairMarking(mV1, mA1), mV3) = analyzeFunc(reduce, mVal)
            val mVal1 = mVal.join(mV1.asMark[v]).join(mV3.asMark[v])
            val ThunkMarking(mVal2) = analyzeThunk(newValue, mVal1)

            val mKey = mKeyVal.get(KeyFieldName).getOrElse(EmptyMarking(eK)).asMark[k]
            val FuncMarking(mA2, _) = analyzeFunc(mapKey, mKey)
            val mPack = analyzeFunc(pack, StringElement.toMarking)

            val FuncMarking(PairMarking(mA3, mA4), _) = analyzeFunc(eq, BooleanElement.toMarking)
            assert(mA3 == mA4)
            Seq((xs, xs.elem.toMarking |/| (All, mA1.asMark[a].join(mA2.asMark[a]).join(mA3.asMark[a]))))
        }

        case IterMethods.join(_ls, _rs,
                _lk @ Def(_: Lambda[a, k]),
                _rk @ Def(_: Lambda[b, _]),
                _cr) =>
          val ls = _ls.asRep[Iter[a]]
          val rs = _rs.asRep[Iter[b]]
          val lk = _lk.asRep[a => k]
          val rk = _rk.asRep[b => k]
          val cr = _cr.asRep[b => b]
          implicit val eA = lk.elem.eDom
          implicit val eK = lk.elem.eRange
          implicit val eB = rk.elem.eDom
          assert(eK == rk.elem.eRange, s"${eK} != ${rk.elem.eRange}")

          val mlk = analyzeFunc(lk, eK.toMarking)
          val mrk = analyzeFunc(rk, eK.toMarking)
          val IterMarking(_, pm) = outMark
          val PairMarking(outL, outR) = pm.asInstanceOf[SliceMarking[_]]
          val mA = mlk.mDom.join(outL.asMark[a])
          val mB = mrk.mDom.join(outR.asMark[b])
          // Cloning can't change the mark
          val _ = analyzeFunc(cr, mB)
          Seq[MarkedSym](
            (thisSym.asRep[Iter[(a,b)]], IterMarking(All, PairMarking(mA, mB))),
            ls.marked(ls.elem.toMarking |/| (All, mA)),
            rs.marked(rs.elem.toMarking |/| (All, mB))
          )

        case IterMethods.reduce(xs: RIter[a]@unchecked, _f @ Def(l: Lambda[_, _]), init: Th[b] @unchecked) =>
          val IterMarking(path, mB_out0) = outMark.asMark[Iter[b]]
          val mB_out = mB_out0.asMark[b]
          val f = _f.asRep[((b,a)) => b]
          implicit val eB = f.elem.eRange
          val mf = analyzeFunc(f, mB_out)
          val PairMarking(mB, mA) = mf.mDom
          val mxs = xs.elem.toMarking |/| (All, mA)
          // TODO not used?
          val _ = analyzeThunk(init, mB_out)
          Seq[MarkedSym](xs.marked(mxs))

        case IterMethods.isEmpty(xs: RIter[a]@unchecked) =>
          val eA = xs.elem.eRow
          val mxs = xs.elem.toMarking |/| (All, EmptyMarking(eA))
          Seq[MarkedSym](xs.marked(mxs))

        case TableIterMethods.byRowids(xs: RIter[a], ys: RIter[b] @unchecked, _f) =>
          val FuncMarking(mB, _) = analyzeFunc(_f.asRep[b => Rowid], RowidElement.toMarking)
          Seq[MarkedSym](xs.marked(outMark.asMark[Iter[a]]), ys.marked(getMark(ys) |/| (All, mB)))

        case Clone(x) =>
          Seq[MarkedSym](x.marked(outMark))

        // Parameter doesn't really depend on its argument
        case _: Parameter[_] | _: ExtraDeps =>
          Seq.empty

        case AdvanceIter(iter: RIter[a] @unchecked, _) =>
          val PairMarking(_, iM) = outMark
          Seq[MarkedSym](iter.asRep[Iter[a]].marked(iM.asMark[Iter[a]]))

        case CursorIterMethods.uniqueByKey(iter, key) =>
          uniqueLike(iter, key)

        case TableIterMethods.uniqueByRowid(iter, key) =>
          uniqueLike(iter, key)

        case CursorIterMethods.uniqueValueByKey(iter, key) =>
          uniqueValueLike(iter, key)

        case TableIterMethods.uniqueValueByRowid(iter, key) =>
          uniqueValueLike(iter, key)

        case _ =>
          super.getInboundMarkings(te, outMark)
      }
    }
  }

  override def createEmptyMarking[T](eT: Elem[T]): SliceMarking[T] = eT match {
    case ie: IterElem[a,_] =>
      implicit val eA = ie.eRow
      IterMarking(None, EmptyMarking(eA)).asMark[T]
    case kie: KernelInputElem[_] =>
      KernelInputMarking.asMark[T]
    case _ =>
      super.createEmptyMarking(eT)
  }

  override def createAllMarking[T](eT: Elem[T]): SliceMarking[T] = eT match {
    case ae: IterElem[a,_] =>
      implicit val eA = ae.eRow
      IterMarking[a](All, AllMarking(eA)).asMark[T]
    case kie: KernelInputElem[_] =>
      KernelInputMarking.asMark[T]
    case _ =>
      super.createAllMarking(eT)
  }

  val KernelInputMarking = AllBaseMarking(kernelInputElement)

  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
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

    case CursorIterMethods.fromKeyWhile(
    IsSliced(xs: Rep[CursorIter[s]] @unchecked, im @ IterMarking(All, mA: SliceMarking[a])), keyValues, op, _p, fakeDep) =>
      val p = _p.asRep[a => Boolean]
      val sp = sliceIn(p, mA).asRep[s => Boolean]
      Sliced(xs.fromKeyWhile(keyValues, op, sp, fakeDep), im.asMark[Iter[a]])

    case CursorIterMethods.fromBeginning(
    IsSliced(xs: Rep[CursorIter[s]] @unchecked, im @ IterMarking(All, mA: SliceMarking[a])), fakeDep) =>
      Sliced(xs.fromBeginning(fakeDep), im.asMark[Iter[a]])

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
    IsSliced(xs: RIter[s] @unchecked, IterMarking(All, sm: SliceMarking[a])),
    _mapKey: RFunc[_,k] @unchecked, _packKey, newValue: Th[v] @unchecked, _reduceValue) =>
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

    case IterMethods.flatMap(IsSliced(xs: RIter[s] @unchecked, IterMarking(All, sm: SliceMarking[a])), _f: RFunc[_, Iter[b]] @unchecked) =>
      // TODO by analogy with map
      val f = _f.asRep[a => Iter[b]]
      ???("TODO: slicing for flatMap")

    case IterMethods.flatMap0or1(IsSliced(xs: RIter[s] @unchecked, IterMarking(All, sm: SliceMarking[a])), _f: RFunc[_, Opt[b]] @unchecked) =>
      val f = _f.asRep[a => Opt[b]]
      ???("TODO: slicing for flatMap0or1")

    case IterMethods.join(
    IsSliced(ls: RIter[s] @unchecked, IterMarking(All, mA: SliceMarking[a])),
    _rs, _lk, rk: RFunc[b,k] @unchecked, _cr) =>
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
    IsSliced(rs: RIter[s] @unchecked, IterMarking(All, mB: SliceMarking[b])),
    lk: RFunc[a,k] @unchecked, _rk, _cr) =>
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
    IsSliced(xs: RIter[s] @unchecked, IterMarking(All, mA: SliceMarking[a])),
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
    IsSliced(xs: RIter[s] @unchecked, im @ IterMarking(All, mA: SliceMarking[a])), _c) =>
      val c = _c.asRep[((a,a)) => Int]
      val sp = sliceIn(c, PairMarking(mA,mA)).asRep[((s,s)) => Int]
      val eS = xs.elem.eRow
      assert(eS == sp.elem.eDom.eFst, s"$eS == ${sp.elem.eDom.eFst}")
      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
      Sliced(xs.sortBy(sp), im.asMark[Iter[a]])

    case IterMethods.sort(
    IsSliced(xs: RIter[s] @unchecked, im @ IterMarking(All, mA: SliceMarking[a])), _c) =>
      val c = _c.asRep[((a,a)) => Boolean]
      val sp = sliceIn(c, PairMarking(mA,mA)).asRep[((s,s)) => Boolean]
      val eS = xs.elem.eRow
      assert(eS == sp.elem.eDom.eFst, s"$eS == ${sp.elem.eDom.eFst}")
      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
      Sliced(xs.sort(sp), im.asMark[Iter[a]])

    case IterMethods.isEmpty(
    IsSliced(xs: RIter[s] @unchecked, IterMarking(All, mA: SliceMarking[a]))) =>
      val eA = mA.elem
      val eS = xs.elem.eRow
      assert(eS == mA.projectedElem, s"$eS == ${mA.projectedElem}")
      xs.isEmpty

    case TableIterMethods.byRowids(
    IsSliced(_xs: RIter[a] @unchecked, im),
    _ys,
    f: RFunc[b, Rowid] @unchecked) =>
      val xs = _xs.asRep[TableIter[a]]
      Sliced(xs.byRowids(_ys.asRep[Iter[b]], f), im.asMark[Iter[a]])

    case TableIterMethods.byRowids(
    xs,
    IsSliced(_ys: RIter[b1] @unchecked, IterMarking(All, mB: SliceMarking[b])),
    _f: RFunc[_, Rowid] @unchecked) =>
      val f = _f.asRep[b => Rowid]
      val ys = _ys.asRep[Iter[b1]]
      val fs = sliceIn(f, mB).asRep[b1 => Rowid]
      xs.byRowids(ys, fs)

    case Clone(IsSliced(p, m)) =>
      Sliced(clone(p), m)


    case Parameter(index, IsSliced(p, m), value) =>
      Parameter(index, p, value)(d.selfType)

    case AdvanceIter(IsSliced(iter: RIter[a] @unchecked, m), counter) =>
      Sliced(AdvanceIter(iter.asRep[Iter[a]], counter), m)

    case CursorIterMethods.uniqueByKey(IsSliced(iter: Rep[CursorIter[a]] @unchecked, m), key) =>
      Sliced(iter.uniqueByKey(key), m)

    case TableIterMethods.uniqueByRowid(IsSliced(iter: Rep[TableIter[a]] @unchecked, m), key) =>
      Sliced(iter.uniqueByRowid(key), m)

    case CursorIterMethods.uniqueValueByKey(IsSliced(iter: Rep[CursorIter[a]] @unchecked, m), key) =>
      Sliced(iter.uniqueValueByKey(key), m)

    case TableIterMethods.uniqueValueByRowid(IsSliced(iter: Rep[TableIter[a]] @unchecked, m), key) =>
      Sliced(iter.uniqueValueByRowid(key), m)

    case _ => super.rewriteDef(d)
  }

}
