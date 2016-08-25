package scalan.sql

import scalan.BaseNestedTests
import scalan.compilation.GraphVizConfig
import scalan.sql.compilation.{RelationConcretizer, RelationToIterBridge, ScalanSqlBridge}
import scalan.sql.parser.{SqlResolver, SqliteResolver}

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
  def testQuery(query: TestQuery) = {
    val scalan = new ScalanSqlExp {}
    import scalan._
    val bridge = tpchBridge(scalan)
    val iterBridge = new RelationToIterBridge[scalan.type](scalan)

    bridge.sqlQueryExp(query.sql) match {
      case relExp: RFunc[Struct, Relation[a]] =>
        val iterExp = iterBridge.relationFunToIterFun(relExp)
        scalan.emitDepGraph(iterExp, prefix, currentTestNameAsFileName)(GraphVizConfig.default)
    }
  }
}

class RelationConcretizerTests extends AbstractSqlBridgeTests {
  def testQuery(query: TestQuery) = {
    val scalan = new ScalanSqlExp {}
    import scalan._
    val bridge = tpchBridge(scalan)
    val concretizer = new RelationConcretizer[scalan.type](bridge)

    bridge.sqlQueryExp(query.sql) match {
      case relExp: RFunc[Struct, Relation[a]] =>
        val plans = concretizer.concretePlans(relExp)
        scalan.emitDepGraph(plans, prefix, currentTestNameAsFileName)(GraphVizConfig.default)
    }
  }
}
