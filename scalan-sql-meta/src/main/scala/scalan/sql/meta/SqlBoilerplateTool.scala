package scalan.sql.meta

import scalan.meta._

object SqlBoilerplateTool extends BoilerplateTool {
  lazy val sqlConfig = CodegenConfig(
    name = "sql",
    srcPath = "scalan-sql-core/src/main/scala",
    entityFiles = List(
      "scalan/sql/Iters.scala",
      "scalan/sql/Relations.scala",
      "scalan/sql/Scannables.scala",
      "scalan/sql/Databases.scala",
      "scalan/sql/KernelInputs.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanStd",
    stagedContextTrait = "ScalanExp",
    entityTypeSynonyms = Map("ROpt" -> "Opt", "Arr" -> "Array")
  )

  override def getConfigs(args: Array[String]) = Seq(sqlConfig)

}
