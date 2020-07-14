name := "Scala-Mungo-Prototype"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalameta" %% "scalameta" % "4.3.18"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test