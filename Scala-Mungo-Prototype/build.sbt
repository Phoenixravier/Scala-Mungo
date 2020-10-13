

scalaVersion := "2.13.3"

description := "Protocol checker for Scala"

ThisBuild / organization := "org.me"

ThisBuild / version := "0.2-SNAPSHOT"

name := "Scala-Mungo-Prototype"

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

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

