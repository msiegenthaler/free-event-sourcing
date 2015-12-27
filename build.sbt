name := "free-event-sourcing"

version := "1.0"

scalaVersion := "2.11.7"
scalacOptions += "-feature"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies += "ch.inventsoft" %% "free-to-compose" % "0.1.0"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.5"