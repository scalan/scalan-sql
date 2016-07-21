package scalan.sql

import scalan.BaseNestedTests
import scalan.compilation.GraphVizConfig
import scalan.sql.compilation.{RelationToIterBridge, ScalanSqlBridge}
import scalan.sql.parser.SqlResolver
import scalan.sql.parser.SqlAST.BasicStringType

abstract class AbstractSqlBridgeTests extends BaseNestedTests {
  def createExpAndGraph(query: TestQuery): Unit

  def tpchBridge(scalan: ScalanSqlExp): ScalanSqlBridge[scalan.type] =
    new ScalanSqlBridge[scalan.type](TPCH.Schema, scalan) {
      override protected def initSqlResolver(p: SqlResolver) = {
        p.registerFunctionType("strftime", BasicStringType)
      }
    }

  describe("TPCH") {
    TPCH.allQueries.foreach {
      case (name, query) =>
        it(name) {
          name match {
              // not supported yet
            case "Q13" | "Q16" =>
              pendingUntilFixed { createExpAndGraph(query) }
            case _ =>
              createExpAndGraph(query)
          }
        }
    }
  }
}

class RelationSqlBridgeTests extends AbstractSqlBridgeTests {
  def createExpAndGraph(query: TestQuery) = {
    val scalan = new ScalanSqlExp {}
    val bridge = tpchBridge(scalan)

    val exp = bridge.sqlQueryExp(query.sql)
    scalan.emitDepGraph(exp, prefix, currentTestNameAsFileName)(GraphVizConfig.default)
  }
}

class IterSqlBridgeTests extends AbstractSqlBridgeTests {
  def createExpAndGraph(query: TestQuery) = {
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
