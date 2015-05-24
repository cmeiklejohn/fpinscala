name := "My Project"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

scalacOptions += "-deprecation"

scalacOptions += "-feature"
