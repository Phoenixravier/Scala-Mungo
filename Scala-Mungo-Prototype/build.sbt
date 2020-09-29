name := "Scala-Mungo-Prototype"

version := "0.2"

scalaVersion := "2.13.3"

description := "my description"

ThisBuild / licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

/*
lazy val root = (project in file("."))
  .settings(
    sbtPlugin := true,
    name := "Scala-Mungo-Prototype",
    publishMavenStyle := false,
    bintrayRepository := "sbt-plugins",
    bintrayOrganization in bintray := None
  )
packageBin in Compile := file(s"${name.value}_${scalaBinaryVersion.value}.jar")
*/


//addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.2")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "io.suzaku" %% "boopickle" % "1.3.2"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.8"

libraryDependencies += "org.antlr" % "stringtemplate" % "3.2"
