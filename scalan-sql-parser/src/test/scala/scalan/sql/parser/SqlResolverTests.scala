package scalan.sql.parser

import scalan.BaseNestedTests
import scalan.sql.TPCH

class SqlResolverTests extends BaseNestedTests {
  describe("Can resolve TPCH queries") {
    val parser = new SqlParser
    val schema = parser.parseDDL(TPCH.DDL)
    val resolver = new SqliteResolver(schema)

    // just verifies that queries can be parsed and resolved
    // TODO add tests that resolution is correct!
    TPCH.allQueries.foreach {
      case (name, query) =>
        it(name) {
          val unresolved = parser.parseSelect(query.sql).operator

          val resolved = resolver.resolveOperator(unresolved)

          // cheating test
          assert(!resolved.toString.contains("Unresolved"))
        }
    }
  }
}
