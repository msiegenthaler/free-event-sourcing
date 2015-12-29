name := "free-event-sourcing"

version := "1.0"

scalaVersion := "2.11.7"
scalacOptions += "-feature"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies += "com.chuusai" %% "shapeless" % "2.2.5"
libraryDependencies += "org.spire-math" %% "cats" % "0.3.0"
