package scalan.sql.meta

import scalan.meta.ScalanAst._
import scalan.meta._

object SqlExpr {
  def unapply(expr: SExpr): Option[(String, String)] = expr match {
    case SApply(SConst(value @ ("sql" | "ddl")), _, argss) =>
      Some((value.toString, argss(0)(0).asInstanceOf[SConst].c.toString))
    case _ => None
  }
}

class SqlEntityFileGenerator(codegen: MetaCodegen, module: SEntityModuleDef, config: CodegenConfig)
  extends EntityFileGenerator(codegen, module, config) {

  val sqlCodegen = new SqlCodegen

  override def extraBody(entity: STraitOrClassDef): String = {
    entity.body.collect { case m: SMethodDef =>
      m.body match {
        case Some(SqlExpr("sql", sql)) =>
          sqlCodegen.generateQuery(sql, m)
        case _ => ""
      }
    }.mkString("\n\n")
  }

  override def extraTraitAbs: String = {
    val sqlDDL = module.methods.map(m =>
      m.body match {
        case Some(SqlExpr("ddl", sql)) => sql
        case _ => ""
      }
    ).mkString

    val sqlSchema = if (sqlDDL.isEmpty) "" else sqlCodegen.generateSchema(sqlDDL)

    val sqlQueries = module.methods.flatMap { m =>
      m.body match {
        case Some(SqlExpr("sql", sql)) =>
          Some(sqlCodegen.generateQuery(sql, m))
        case _ => None
      }
    }

    (sqlSchema :: sqlQueries).mkString("\n")
  }
}

object SqlBoilerplateTool extends BoilerplateTool {
  lazy val sqlConfig = CodegenConfig(
    name = "sql",
    srcPath = "scalan-sql-core/src/main/scala",
    entityFiles = List(
      "scalan/sql/Iters.scala",
      "scalan/sql/Relations.scala"
    ),
    baseContextTrait = "Scalan",
    seqContextTrait = "ScalanStd",
    stagedContextTrait = "ScalanExp",
    entityTypeSynonyms = Map()
  )

  override def getConfigs(args: Array[String]) = Seq(sqlConfig)
}

class SqlEntityManagement(config: CodegenConfig) extends EntityManagement(config) {
  override def createFileGenerator(codegen: MetaCodegen, module: SEntityModuleDef, config: CodegenConfig) = {
    new SqlEntityFileGenerator(codegen, module, config)
  }
}
