name := "free-event-sourcing"

version := "1.0"

scalaVersion := "2.12.1"
scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"
javacOptions in Compile ++= Seq("-source", "1.8", "-target", "1.8")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.13"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.4.13"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"


import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

SbtScalariform.defaultScalariformSettings
ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)


enablePlugins(VersionEyePlugin)
propertiesPath in versioneye := ".versioneye.properties"
baseUrl in versioneye := "https://www.versioneye.com"
apiPath in versioneye := "/api/v2"
publishCrossVersion in versioneye := true
