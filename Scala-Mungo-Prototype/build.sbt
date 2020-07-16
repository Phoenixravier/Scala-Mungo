name := "Scala-Mungo-Prototype"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalameta" %% "scalameta" % "4.3.18"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.8"

libraryDependencies += "org.antlr" % "stringtemplate" % "3.2"

//libraryDependencies += "antlr" % "antlr" % "2.7.7"

//libraryDependencies += "com.simplytyped" % "sbt-antlr4" % "0.8.2"