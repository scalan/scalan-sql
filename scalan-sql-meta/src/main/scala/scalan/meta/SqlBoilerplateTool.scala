package scalan.sql.meta

import scalan.meta._
import scalan.meta.ScalanAst.SEntityModuleDef

object SqlCodegen extends MetaCodegen {
}

class SqlEntityFileGenerator(codegen: MetaCodegen, module: SEntityModuleDef, config: CodegenConfig)
  extends EntityFileGenerator(codegen, module, config) {
}

class SqlEntityManagement(config: CodegenConfig) extends EntityManagement(config) {
  override def getCodegen = SqlCodegen
  override def createFileGenerator(codegen: MetaCodegen, module: SEntityModuleDef, config: CodegenConfig) = {
    new SqlEntityFileGenerator(SqlCodegen, module, config)
  }
}

object SqlBoilerplateTool extends BoilerplateTool {
  lazy val sqlConfig = CodegenConfig(
    name = "sql",
    srcPath = "scalan-sql-core/src/main/scala",
    entityFiles = List(
      "scalan/sql/Iters.scala"//,
//      "scalan/sql/aspendb/Responses.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanStd",
    stagedContextTrait = "ScalanExp",
    extraImports = List(
        "scala.reflect.runtime.universe._",
        "scalan.common.Default"),
    entityTypeSynonyms = Map()
  )

  override def getConfigs(args: Array[String]) = Seq(sqlConfig)

  override def main(args: Array[String]) = {
    val configs = getConfigs(args)
    configs.foreach { config => new SqlEntityManagement(config).generateAll() }
  }
}
