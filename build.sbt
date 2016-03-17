name := "free-event-sourcing"

version := "1.0"

scalaVersion := "2.11.8"
scalacOptions += "-feature"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.0"
libraryDependencies += "org.typelevel" %% "cats" % "0.4.1"


import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)