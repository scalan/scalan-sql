package scalan.sql.parser

import scalan.BaseNestedTests
import scalan.sql.{TPCH, TestQuery}

class SqlResolverTests extends BaseNestedTests {
  val parser = new SqlParser
  val schema = parser.parseDDL(TPCH.DDL)
  val resolver = new SqliteResolver(schema)

  describe("Can resolve TPCH queries") {
    // just verifies that queries can be parsed and resolved
    // TODO add tests that resolution is correct!
    TPCH.allQueries.foreach {
      case (name, query) =>
        it(name) {
          assertQueryIsResolved(query)
        }
    }
  }

  describe("accessing non-selected columns:") {
    it("order by and filter") {
      assertQueryIsResolved(
        """SELECT n_name FROM nation
          |WHERE n_comment <> ''
          |ORDER BY n_nationkey""".stripMargin)
    }

    it("group by and order by on same column") {
      assertQueryIsResolved(
        """SELECT MAX(n_nationkey) FROM nation
          |GROUP BY n_regionkey
          |ORDER BY n_regionkey
        """.stripMargin)
    }

    it("join and order by") {
      assertQueryIsResolved(
        """SELECT (l_partkey + o_custkey) AS s
          |FROM lineitem JOIN orders ON l_orderkey = o_orderkey
          |WHERE o_orderdate > '1998-07-12'
          |ORDER BY o_custkey
        """.stripMargin)
    }
  }

  it("filter on projected columns") {
    assertQueryIsResolved(
      """select n_regionkey + n_nationkey as key_sum from nation
        |where key_sum > 10
        |order by key_sum""".stripMargin
    )
  }

  it("master-detail query") {
    pendingUntilFixed(assertQueryIsResolved(
      """select o.*, count(l_linenumber) as itemsNo, sum(l_extendedprice) as amt from orders as o
        |join lineitem  on l_orderkey = o_orderkey
        |where o_orderdate > '1996-10-11'
        |group by o_orderkey""".stripMargin
    ))
  }

  def assertQueryIsResolved(query: TestQuery): Unit = {
    val unresolved = parser.parseSelect(query.sql, turnLiteralsIntoParameters = true).operator

    val resolved = resolver.resolveOperator(unresolved)

    assert(!resolved.toString.contains("[Unresolved]"))

    // check resolution correctness manually for now
    println(s"Original SQL:\n${query.sql}\n\nUnresolved:\n$unresolved\n\nResolved:\n$resolved\n\n")
  }
}
