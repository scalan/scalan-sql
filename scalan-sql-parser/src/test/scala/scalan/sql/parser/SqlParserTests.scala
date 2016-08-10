package scalan.sql.parser

import scalan.BaseNestedTests
import scalan.sql.TPCH

class SqlParserTests extends BaseNestedTests {
  describe("Can parse TPCH queries") {
    val parser = new SqlParser
    parser.parseDDL(TPCH.DDL)

    // just verifies that queries can be parsed
    TPCH.allQueries.foreach {
      case (name, query) =>
        it(name) {
          parser.parseSelect(query.sql)
        }
    }
  }
}
