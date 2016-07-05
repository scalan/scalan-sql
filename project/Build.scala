import sbt._
import sbt.Keys._

object ScalanSqlRootBuild extends Build {
  lazy val IntegrationTest = config("it") extend(Test)

  val commonDeps = libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.5" % "test")

  val testSettings = Defaults.itSettings ++ Seq(
    // needed thanks to http://stackoverflow.com/questions/7898273/how-to-get-logging-working-in-scala-unit-tests-with-testng-slf4s-and-logback
    parallelExecution in Test := false,
    parallelExecution in IntegrationTest := false,
    publishArtifact in Test := true,
    javaOptions in Test ++= Seq("-Xmx10G", "-Xms5G"),
    fork in IntegrationTest := true,
    publishArtifact in(Test, packageDoc) := false
  )

  val buildSettings = Seq(
    organization := "com.huawei.scalan",
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq(
      "-unchecked", "-deprecation",
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:existentials",
      "-language:postfixOps"))

  lazy val noPublishingSettings = Seq(
    publishArtifact := false,
    publish := {},
    publishLocal := {})

  override lazy val settings = super.settings ++ buildSettings

  lazy val commonSettings =
    buildSettings ++ testSettings ++
      Seq(
      resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
      publishTo := {
        val nexus = "http://10.122.85.37:9081/nexus/"
        if (version.value.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at (nexus + "content/repositories/snapshots"))
        else
          Some("releases" at (nexus + "content/repositories/releases"))
      },
      commonDeps)

  implicit class ProjectExt(p: Project) {
    def allConfigDependency = p % "compile->compile;test->test;it->test"

    def addTestConfigsAndCommonSettings =
      p.configs(IntegrationTest).settings(commonSettings: _*)
  }

  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.11.2")
  
  def scalanDependency(name: String) = "com.huawei.scalan" %% name % "0.3.0-SNAPSHOT"

  lazy val scalanMeta        = scalanDependency("scalan-meta")
  lazy val scalanCommon      = scalanDependency("scalan-common")
  lazy val scalanCore        = scalanDependency("scalan-core")
  lazy val scalanLua         = scalanDependency("scalan-lua-backend-core")

  lazy val meta = Project(
    id = "scalan-sql-meta",
    base = file("scalan-sql-meta")).addTestConfigsAndCommonSettings.
    settings(fork in run := true, libraryDependencies ++= Seq(scalanMeta))

//  lazy val rt = Project(
//    id = "scalan-sql-rt",
//    base = file("scalan-sql-rt")).addTestConfigsAndCommonSettings.
//    settings(fork in run := true, libraryDependencies ++= Seq(scalanCommon))

//  lazy val parser = Project(
//    id = "scalan-sql-parser",
//    base = file("scalan-sql-parser")).addTestConfigsAndCommonSettings.
//    settings(libraryDependencies ++= Seq(scalanMeta, scalanCommon,
//      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"))

  lazy val core = Project(
    id = "scalan-sql-core",
    base = file("scalan-sql-core")).addTestConfigsAndCommonSettings.
    settings(libraryDependencies ++= Seq(
      scalanCommon, scalanCommon % "test" classifier "tests",
      scalanCore, scalanCore % "test" classifier "tests",
      scalanLua, scalanLua % "test" classifier "tests"
    ))

  lazy val root = Project(
    id = "scalan-sql",
    base = file(".")).addTestConfigsAndCommonSettings
      .aggregate(meta, core)
      .settings(publishArtifact := false)

  publishArtifact in Test := true

  // do not publish docs for snapshot versions
  publishArtifact in packageDoc := !version.value.trim.endsWith("SNAPSHOT")

  publishTo in ThisBuild := {
    val nexus = "http://10.122.85.37:9081/nexus/"
    if (version.value.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at (nexus + "content/repositories/snapshots"))
    else
      Some("releases" at (nexus + "content/repositories/releases"))
  }
}
