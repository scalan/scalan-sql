package scalan.sql

import scalan.staged.Slicing

trait SqlSlicing extends Slicing { ctx: ScalanSqlExp =>
  protected def createSliceAnalyzer: SliceAnalyzer = new SqlSliceAnalyzer

  class SqlSliceAnalyzer extends SliceAnalyzer {
    import KeyPath._

    override def getInboundMarkings[T](te: TableEntry[T], outMark: SliceMarking[T]): MarkedSyms = {
      val thisSym = te.sym
      val d = te.rhs
      d match {
        case IterMethods.map(_xs, f: RFunc[a, b]) => outMark match {
          case IterMarking(_, mB_out) =>
            val xs = _xs.asRep[Iter[a]]
            val mOutMark = mB_out.asMark[b]
            val lm = analyzeFunc(f, mOutMark)
            Seq((xs, getMark(xs) |/| (All, lm.mDom)))
        }

        case IterMethods.flatMap(_xs, f: RFunc[a, Iter[b]]) => outMark match {
          case IterMarking(_, mB_out) =>
            val xs = _xs.asRep[Iter[a]]
            val mOutMark = IterMarking(All, mB_out.asMark[b])
            val lm = analyzeFunc(f, mOutMark)
            Seq((xs, getMark(xs) |/| (All, lm.mDom)))
        }

        case IterMethods.sortBy(_xs, _f) => outMark match {
          case IterMarking(_, mA: SliceMarking[a]) =>
            val xs = _xs.asRep[Iter[a]]
            val f = _f.asRep[((a,a)) => Int]
            val mInt = element[Int].toMarking
            val fm = analyzeFunc(f, mInt)
            val PairMarking(mD,_) = fm.mDom
            val mDom = mD.asMark[a].join(mA)
            val mxs = getMark(xs) |/| (All, mDom)
            Seq[MarkedSym](
              (xs, mxs), (thisSym.asRep[Iter[a]], mxs),
              f.marked(FuncMarking(PairMarking(mDom, mDom), mInt)))
        }

        case IterMethods.sort(_xs, _f) => outMark match {
          case IterMarking(_, mA: SliceMarking[a]) =>
            val xs = _xs.asRep[Iter[a]]
            val f = _f.asRep[((a,a)) => Boolean]
            val mBoolean = element[Boolean].toMarking
            val fm = analyzeFunc(f, mBoolean)
            val PairMarking(mD,_) = fm.mDom
            val mDom = mD.asMark[a].join(mA)
            val mxs = getMark(xs) |/| (All, mDom)
            Seq[MarkedSym](
              (xs, mxs), (thisSym.asRep[Iter[a]], mxs),
              f.marked(FuncMarking(PairMarking(mDom, mDom), mBoolean)))
        }

        case IterMethods.partialSort(_xs, _f1, _f2) => outMark match {
          case IterMarking(_, mA: SliceMarking[a]) =>
            val xs = _xs.asRep[Iter[a]]
            val f1 = _f1.asRep[((a,a)) => Boolean]
            val f2 = _f2.asRep[((a,a)) => Boolean]
            val mBoolean = element[Boolean].toMarking
            val f1m = analyzeFunc(f1, mBoolean)
            val PairMarking(mD1,_) = f1m.mDom
            val f2m = analyzeFunc(f2, mBoolean)
            val PairMarking(mD2,_) = f2m.mDom
            val mDom = mD1.asMark[a].join(mD2.asMark[a]).join(mA)
            val mxs = getMark(xs) |/| (All, mDom)
            Seq[MarkedSym](
              (xs, mxs), (thisSym.asRep[Iter[a]], mxs),
              f1.marked(FuncMarking(PairMarking(mDom, mDom), mBoolean)))
        }

        case IterMethods.filter(_xs, f: RFunc[a, Boolean] @unchecked) => outMark match {
          case IterMarking(_, inner) =>
            val xs = _xs.asRep[Iter[a]]
            val mOutMark = inner.asMark[a]
            val mRange = element[Boolean].toMarking
            val mf = analyzeFunc(f, mRange)
            val mDom = mf.mDom.join(mOutMark)
            val mxs = getMark(xs) |/| (All, mDom)
            Seq[MarkedSym]((xs, mxs), (thisSym.asRep[Iter[a]], mxs), f.marked(FuncMarking(mDom, mRange)))
        }

        case IterMethods.takeWhile(_xs, f: RFunc[a, Boolean] @unchecked) => outMark match {
          case IterMarking(_, inner) =>
            val xs = _xs.asRep[Iter[a]]
            val mOutMark = inner.asMark[a]
            val mRange = element[Boolean].toMarking
            val mf = analyzeFunc(f, mRange)
            val mDom = mf.mDom.join(mOutMark)
            val mxs = getMark(xs) |/| (All, mDom)
            Seq[MarkedSym]((xs, mxs), (thisSym.asRep[Iter[a]], mxs), f.marked(FuncMarking(mDom, mRange)))
        }

        case CursorIterMethods.seekIndex(xs: Rep[CursorIter[a]] @unchecked, keyValues, op) =>
          Seq[MarkedSym](xs -> outMark.asMark[Iter[a]])

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

            val mVal = mKeyVal.get("val").getOrElse(EmptyMarking(eV)).asMark[v]
            val FuncMarking(PairMarking(mV1, mA1), mV3) = analyzeFunc(reduce, mVal)
            val mVal1 = mVal.join(mV1.asMark[v]).join(mV3.asMark[v])
            val ThunkMarking(mVal2) = analyzeThunk(newValue, mVal1)

            val mKey = mKeyVal.get("key").getOrElse(EmptyMarking(eK)).asMark[k]
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

            val mVal = mKeyVal.get("val").getOrElse(EmptyMarking(eV)).asMark[v]
            val FuncMarking(PairMarking(mV1, mA1), mV3) = analyzeFunc(reduce, mVal)
            val mVal1 = mVal.join(mV1.asMark[v]).join(mV3.asMark[v])
            val ThunkMarking(mVal2) = analyzeThunk(newValue, mVal1)

            val mKey = mKeyVal.get("key").getOrElse(EmptyMarking(eK)).asMark[k]
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

        case Clone(x) =>
          Seq[MarkedSym](x.marked(outMark))

        // Parameter doesn't really depend on its argument
        case _: Parameter[_] | _: ExtraDeps =>
          Seq.empty

        case _ =>
          super.getInboundMarkings(te, outMark)
      }
    }
  }

  override def createEmptyMarking[T](eT: Elem[T]): SliceMarking[T] = eT match {
    case ie: IterElem[a,_] =>
      implicit val eA = ie.eRow
      IterMarking(KeyPath.None, EmptyMarking(eA)).asMark[T]
    case kie: KernelInputElem[_] =>
      KernelInputMarking.asMark[T]
    case _ =>
      super.createEmptyMarking(eT)
  }

  override def createAllMarking[T](eT: Elem[T]): SliceMarking[T] = eT match {
    case ae: IterElem[a,_] =>
      implicit val eA = ae.eRow
      IterMarking[a](KeyPath.All, AllMarking(eA)).asMark[T]
    case kie: KernelInputElem[_] =>
      KernelInputMarking.asMark[T]
    case _ =>
      super.createAllMarking(eT)
  }

  val KernelInputMarking = AllBaseMarking(kernelInputElement)

  case class SlicedIter[A, B](source: RIter[B], override val innerMark: SliceMarking[A])
    extends Sliced1[A, B, Iter](IterMarking[A](KeyPath.All, innerMark)) {
    override def toString = s"SlicedIter[${innerMark.elem.name}]($source)"
    override def equals(other: Any) = other match {
      case s: SlicedIter[_, _] => source == s.source && innerMark == s.innerMark
      case _ => false
    }
  }

  override def rewriteDef[T](d: Def[T]): Exp[_] = d match {
    case Clone(IsSliced(p, m)) =>
      Sliced(clone(p), m)
    case Parameter(index, IsSliced(p, m), value) =>
      Parameter(index, p, value)(d.selfType)

    case _ => super.rewriteDef(d)
  }

}
