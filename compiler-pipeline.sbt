name := "Arcon Compiler Pipeline"

organization := "se.kth.cda"


version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.8"

scalacOptions ++= Seq("-deprecation","-feature")

libraryDependencies += "se.kth.cda" %% "arc" % "0.1.0-SNAPSHOT"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.+"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.+" % "test"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.5"

