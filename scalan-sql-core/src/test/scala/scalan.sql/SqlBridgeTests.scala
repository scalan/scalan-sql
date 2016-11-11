package scalan.sql

import scalan.BaseNestedTests
import scalan.compilation.GraphVizConfig
import scalan.sql.compilation.{RelationToIterBridge, ScalanSqlBridge}
import scalan.sql.parser.SqliteResolver

abstract class AbstractSqlBridgeTests extends BaseNestedTests {
  def testQuery(query: TestQuery): Unit

  def tpchBridge(scalan: ScalanSqlExp): ScalanSqlBridge[scalan.type] =
    new ScalanSqlBridge[scalan.type](TPCH.DDL, scalan) {
      override lazy val resolver = new SqliteResolver(schema)
    }

  describe("TPCH") {
    TPCH.allQueries.foreach {
      case (name, query) =>
        it(name) {
          name match {
              // too many joins for now
            case "Q2" | "Q8" =>
              pending
              testQuery(query)
              // not supported yet
            case "Q13" | "Q16" =>
              pendingUntilFixed { testQuery(query) }
            case _ =>
              testQuery(query)
          }
        }
    }
  }

  it("project and filter") {
    testQuery(
      """select l_returnflag, l_linestatus
        |from lineitem
        |where l_quantity > 5
      """.stripMargin)
  }

  it("simple aggregate") {
    testQuery(
      """select
        |  sum(l_quantity) as sum_qty,
        |  avg(l_extendedprice) as avg_price
        |from lineitem
        |where
        |  l_quantity > 5""".stripMargin)
  }

  it("group by") {
    testQuery(
      """select
        |    l_returnflag,
        |    sum(l_quantity) as sum_qty
        |from
        |    lineitem
        |group by
        |    l_returnflag""".stripMargin)
  }

  it("filter comparing columns from different tables") {
    testQuery(
      """select l_orderkey
        |from orders join lineitem on l_orderkey = o_orderkey
        |where o_orderdate < l_shipdate""".stripMargin)
  }

  // FIXME need to rework SqlResolver to handle these cases
  it("order by and filter on non-selected columns") {
    pendingUntilFixed {
      testQuery(
        """select n_name from nation
          |where n_comment <> ''
          |order by n_nationkey""".stripMargin)
    }
  }

  it("filter on projected columns") {
    pendingUntilFixed {
      testQuery(
        """select n_regionkey + n_nationkey as key_sum from nation
          |where key_sum > 10
          |order by key_sum""".stripMargin)
    }
  }

  describe("order by index") {
    it("with project") {
      // must be a full (covering) index scan, no sorting
      testQuery("select c_name, c_custkey from customer order by c_name")
    }

    it("inverted") {
      // must be a full (non-covering) reverse index scan, no sorting
      testQuery("select c_name, c_custkey, c_address from customer order by c_name desc")
    }
  }

  it("mapReduce with empty value") {
    testQuery(
      """select
        |    l_orderkey
        |from
        |    orders join lineitem on l_orderkey = o_orderkey
        |where
        |    o_orderdate < '1995-03-04'
        |group by
        |    l_orderkey""".stripMargin)
  }

  it("correlated subquery") {
    testQuery(
      """select
        |    p_size
        |from
        |    part
        |where
        |    p_size = (
        |        select
        |            max(l_linenumber)
        |        from
        |            lineitem
        |        where
        |            l_partkey = p_partkey)""".stripMargin)
  }

  describe("star works") {
    // TODO convert manual checks to real tests
    it("on single table") {
      // check this produces identity function with correct type
      testQuery("select * from lineitem")
    }

    it("on single table with filter") {
      // check this produces correct function with correct type (no `map` on Iters)
      testQuery("select * from lineitem where l_linenumber = 1")
    }

    it("on join") {
      // check that we get Relation[Nation ++ Region], not Relation[(Nation, Region)]
      testQuery("select * from nation join region on n_regionkey = r_regionkey")
    }
  }
}

class RelationSqlBridgeTests extends AbstractSqlBridgeTests {
  def testQuery(query: TestQuery) = {
    val scalan = new ScalanSqlExp {}
    val bridge = tpchBridge(scalan)

    val exp = bridge.sqlQueryExp(query.sql)
    scalan.emitDepGraph(exp, prefix, currentTestNameAsFileName)(GraphVizConfig.default)
  }
}

class IterSqlBridgeTests extends AbstractSqlBridgeTests {
  // TODO run a single pass instead
  def testQuery(query: TestQuery) = {
    var startInvoking = false
    val scalan = new ScalanSqlExp {
      override def invokeAll = startInvoking
    }
    import scalan._
    val bridge = tpchBridge(scalan)
    val iterBridge = new RelationToIterBridge[scalan.type](scalan)

    bridge.sqlQueryExp(query.sql) match {
      case relExp: RFunc[KernelInput, Relation[a]] =>
        startInvoking = true
        val iterExp = iterBridge.relationFunToIterFun(relExp)
        scalan.emitDepGraph(iterExp, prefix, currentTestNameAsFileName)(GraphVizConfig.default)
    }
  }
}
