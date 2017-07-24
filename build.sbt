name := "gnasher"

version := "1.0"

scalaVersion := "2.11.8"

// Test deps
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.11.3"
libraryDependencies += "org.scala-graph" %% "graph-dot" % "1.11.0"
