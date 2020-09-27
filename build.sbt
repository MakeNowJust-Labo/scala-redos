Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubOwner := "MakeNowJust-Labo"
ThisBuild / githubRepository := "scala-redos"

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Wunused"
)

// Scalafix config:
ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.4.1"
ThisBuild / scalafixDependencies += "com.github.vovapolu" %% "scaluzzi" % "0.1.14"

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
    Compile / console / scalacOptions -= "-Wunused",
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.1" cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % "1.7.1" % Provided cross CrossVersion.full
    ),
    // Set URL mapping of scala standard API for Scaladoc.
    autoAPIMappings := true,
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // Dependencies:
    libraryDependencies += "com.ibm.icu" % "icu4j" % "67.1",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.0",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30",
    libraryDependencies += "guru.nidi" % "graphviz-java" % "0.17.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
    // Settings for test:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )
