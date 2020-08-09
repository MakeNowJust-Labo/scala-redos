Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "redos",
    organization := "codes.quine.labo",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import scala.util.chaining._
      |import scala.util.{Try, Success, Failure}
      |import scala.collection.MultiDict
      |
      |import codes.quine.labo.redos._
      |import codes.quine.labo.redos.automaton._
      |import codes.quine.labo.redos.regexp._
      |import codes.quine.labo.redos.util._
      |
      |def render[T: GraphRenderer](t: T, filename: String): Unit = {
      |  import java.io.File
      |  import guru.nidi.graphviz.engine.Format
      |  GraphRenderer.graphviz(t).render(Format.SVG).toFile(new File(filename))
      |}
      """.stripMargin,
    // Set URL mapping of scala standard API for Scaladoc.
    autoAPIMappings := true,
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // dependencies:
    libraryDependencies += "com.ibm.icu" % "icu4j" % "67.1",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.0",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30",
    libraryDependencies += "guru.nidi" % "graphviz-java" % "0.17.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
    // test dependencies:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )
