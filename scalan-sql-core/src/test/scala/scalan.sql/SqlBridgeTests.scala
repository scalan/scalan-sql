package scalan.sql

import scalan.BaseNestedTests
import scalan.compilation.GraphVizConfig
import scalan.sql.compilation.ScalanSqlBridge
import scalan.sql.parser.SqlResolver
import scalan.sql.parser.SqlAST.BasicStringType

class SqlBridgeTests extends BaseNestedTests {
  describe("TPCH") {
    TPCH.allQueries.foreach {
      case (name, query) =>
        it(name) {
          val scalan = new ScalanSqlExp {}
          val bridge = new ScalanSqlBridge[scalan.type](TPCH.Schema, scalan) {
            override protected def initSqlResolver(p: SqlResolver) = {
              p.registerFunctionType("strftime", BasicStringType)
            }
          }

          def createExpAndGraph() = {
            val exp = bridge.sqlQueryExp(query.sql)
            scalan.emitDepGraph(exp, prefix, currentTestNameAsFileName)(GraphVizConfig.default)
          }

          name match {
              // not supported yet
            case "Q13" | "Q16" =>
              pendingUntilFixed { createExpAndGraph() }
            case _ =>
              createExpAndGraph()
          }
        }
    }
  }
}
