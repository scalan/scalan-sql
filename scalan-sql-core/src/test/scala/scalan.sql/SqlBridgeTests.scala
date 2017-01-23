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
            case "Q13" | "Q16" |
              // problems with resolution (investigate later)
              "Q11" | "Q15"
            =>
              pendingUntilFixed { testQuery(query) }
            case _ =>
              testQuery(query)
          }
        }
    }
  }

  it("simple rowid access") {
    // verify that IterSqlBridgeTests version is uniqueByRowid->map, not uniqueValueByRowid->...->ExpConditionalIter
    testQuery("SELECT o_custkey FROM orders WHERE o_orderkey = 100")
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
    // verify that IterSqlBridgeTests version uses uniqueValueByKey and then flatMap0or1
    testQuery(
      """select l_orderkey
        |from orders join lineitem on l_orderkey = o_orderkey
        |where o_orderdate < l_shipdate""".stripMargin)
  }

  it("attribute reordering") {
    // verify there is no map in graph
    testQuery("""SELECT o_comment, o_custkey FROM orders""")
  }

  describe("accessing non-selected columns:") {
    it("order by and filter") {
      testQuery(
        """SELECT n_name FROM nation
          |WHERE n_comment <> ''
          |ORDER BY n_nationkey""".stripMargin)
    }

    it("group by and order by on same column") {
      testQuery(
        """SELECT MAX(n_nationkey) FROM nation
          |GROUP BY n_regionkey
          |ORDER BY n_regionkey
        """.stripMargin)
    }

    it("join and order by") {
      // verify no xs.flatMap(x => ...; y.map(f)).map(g) sequence
      testQuery(
        """SELECT (l_partkey + o_custkey) AS s
          |FROM lineitem JOIN orders ON l_orderkey = o_orderkey
          |WHERE o_orderdate > '1998-07-12'
          |ORDER BY o_custkey
        """.stripMargin)
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

    it("with a join") {
      testQuery(
        """select sum(l_linenumber + o_custkey)
          |from orders join lineitem
          |on l_orderkey = o_orderkey
          |where o_custkey < 1000 and o_totalprice < 10000.0
          |group by o_custkey
          |order by o_custkey
        """.stripMargin)
    }

    it("with a join inverted") {
      testQuery(
        """select sum(l_linenumber + o_custkey)
          |from orders join lineitem
          |on l_orderkey = o_orderkey
          |where o_custkey < 1000 and o_totalprice < 10000.0
          |group by o_custkey
          |order by o_custkey desc
        """.stripMargin)
    }
  }

  describe("vacuous order by") {
    // queries where results have at most one row and so shouldn't contain sort
    it("rowid") {
      // TODO currently o_custkey is read from the table even though it won't be used in the end,
      // but fixing this is non-trivial and low priority
      testQuery("SELECT o_totalprice FROM orders WHERE o_orderkey = 1000 ORDER BY o_custkey")
    }

    it("join and aggregate") {
      pendingUntilFixed {
        // there is currently a resolution error
        testQuery(
          """select sum(l_linenumber + o_custkey)
            |from orders join lineitem
            |on l_orderkey = o_orderkey
            |where o_custkey < 1000 and o_totalprice < 10000.0
            |order by o_custkey
          """.stripMargin)
      }
    }
  }

  describe("partial sorting") {
    it("on primary key") {
      // must be full table scan, then partial sort on c_name with equality on c_custkey
      testQuery("select c_name, c_custkey from customer order by c_custkey, c_name")
    }

    it("common prefix") {
      // must be full index scan on lineitem_pk _or_ lineitem_order_fk, then partial sort on l_partkey
      testQuery("select * from lineitem order by l_orderkey, l_partkey")
    }
  }

  describe("ordered aggregation") {
    it("full order") {
      // must be full index scan, then partialMapReduce with K = Unit
      testQuery("select sum(l_extendedprice), l_orderkey from lineitem group by l_orderkey")
    }

    it("common prefix") {
      // must be full index scan, then partialMapReduce on l_partkey, order by l_partkey
      testQuery(
        """select sum(l_extendedprice), l_orderkey, l_partkey
          |from lineitem
          |group by l_orderkey, l_partkey
          |order by l_orderkey, l_partkey""".stripMargin)
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

  describe("? bind parameters work") {
    it("single parameter") {
      testQuery("SELECT * FROM customer WHERE c_custkey = ?")
    }

    it("mix of parameters and literals") {
      testQuery(
        """SELECT * FROM
          |nation JOIN region ON n_regionkey = r_regionkey
          |JOIN supplier ON n_nationkey = s_nationkey
          |WHERE s_suppkey = ? AND n_nationkey = 1 AND r_name = ?""".stripMargin)
    }
  }

  it("select from table without using any columns") {
    testQuery("select 1 from nation")
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
