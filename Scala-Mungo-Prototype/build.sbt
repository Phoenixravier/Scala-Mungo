

scalaVersion := "2.13.3"

description := "Protocol checker for Scala"

ThisBuild / organization := "org.me"

//ThisBuild / version := "1.1"
ThisBuild / version := "0.9.1-SNAPSHOT"

name := "Scala-Mungo-Prototype"
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
//ThisBuild / licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/Aliceravier/Scala-Mungo"))
/*
pomExtra :=
  <scm>
    <connection>
      scm:git:git://github.com/Aliceravier/Scala-Mungo.git
    </connection>
    <url>
      https://github.com/Aliceravier/Scala-Mungo
    </url>
  </scm>
    <developers>
      <developer>
        <id>aliceravier</id>
        <name>Alice Ravier</name>
        <email>aliceravier@yahoo.co.uk</email>
      </developer>
    </developers>

publishTo in ThisBuild := Some(
  "Project Bintray" at
    "https://bintray.com/aliceravier")
credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
publishMavenStyle := true
*/

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

//addCompilerPlugin("org.wartremover" %% "wartremover" % "2.4.11" cross CrossVersion.full)
//scalacOptions += "-P:wartremover:traverser:org.wartremover.warts.Unsafe"



