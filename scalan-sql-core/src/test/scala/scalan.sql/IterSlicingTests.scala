package scalan.sql

import scalan.common.Lazy
import scalan.staged.AbstractSlicingTests

class IterSlicingTests extends AbstractSlicingTests {

  class Ctx extends super.Ctx with ScalanSqlExp with SqlSlicing {
    override def createSliceAnalyzer = super.createSliceAnalyzer

    lazy val funMapIter = fun { xs: Rep[Iter[Struct]] =>
      xs.map(funMap)
      }(Lazy(iterElement(eJoined)), iterElement(pairElement(eInt, eInt)))

    lazy val funFilterIter = fun { xs: Rep[Iter[Struct]] =>
      xs.filter(funPred)
      }(Lazy(iterElement(eJoined)), iterElement(eJoined))

    lazy val funMapReduce = fun { xs: Rep[Iter[Struct]] =>
      xs.mapReduce(funKey, funKey, Thunk { 0 }, funReduce)
      }(Lazy(iterElement(eJoined)), iterElement(eKV))

    lazy val funJoin = fun { in: Rep[(Iter[Struct], Iter[Struct])] =>
      val Pair(ls, rs) = in
      ls.join(rs, funOneField, funOneField, cloneFun(eIn))
      }(Lazy(pairElement(iterElement(eIn),iterElement(eIn))),
        iterElement(pairElement(eIn, eIn)))

    lazy val funIterReduce = fun { in: Rep[Iter[Struct]] =>
      in.reduce(fun ({ in: Rep[(Int, Struct)] =>
        val Pair(i, s) = in
        i + s.get[Int]("a")
      })(Lazy(pairElement(eInt, eIn)), eInt), Thunk(0))
      }(Lazy(iterElement(eIn)), iterElement(eInt))
  }

  class TestHelper(override val ctx: Ctx) extends super.TestHelper(ctx)

  test("SliceAnalizer") {
    val ctx = new TestHelper(new Ctx)
    import ctx.compiler.scalan._
    import ctx.testFuncMark

    testFuncMark(funMapIter, IterMarking(KeyPath.All, PairMarking(eInt.toMarking, EmptyMarking(eInt))),
      IterMarking(KeyPath.All,
        StructMarking(
          Seq("a" -> AllMarking(eInt),
              "d" -> StructMarking(
                       Seq("e" -> AllMarking(eInt))
                     )(eNested)
          )
        )(eJoined)
      )
    )
    val mSliced = StructMarking(
      Seq("a" -> AllMarking(eInt),
        "d" -> StructMarking(
          Seq("e" -> AllMarking(eInt))
        )(eNested)
      )
    )(eJoined)
    testFuncMark(funFilterIter, IterMarking(KeyPath.All, mSliced),
      IterMarking(KeyPath.All, mSliced)
    )
    testFuncMark(funMapReduce, IterMarking(KeyPath.All, eKV.toMarking),
      IterMarking(KeyPath.All,
        StructMarking(
          Seq("b" -> AllMarking(eString),
            "d" -> StructMarking(
              Seq("e" -> AllMarking(eInt))
            )(eNested)
          )
        )(eJoined)
      )
    )
  }

  test("FuncSlicing") {
    val ctx = new TestHelper(new Ctx)
    import ctx.testFuncSlice
    import ctx.compiler.scalan._
    import KeyPath._

    testFuncSlice("7", funMapIter, IterMarking(All, PairMarking(eInt.toMarking, EmptyMarking(eInt))))
    testFuncSlice("8", funFilterIter, IterMarking(All, AllMarking(eJoined)))
    testFuncSlice("9", funMapReduce, iterElement(eKV).toMarking)
    testFuncSlice("10", funJoin, IterMarking(All, PairMarking(mSlicedIn, mSlicedIn)))
    testFuncSlice("11", funIterReduce, IterMarking(All, eInt.toMarking))
  }
}
