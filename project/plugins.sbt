logLevel := Level.Warn

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M15")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

addSbtPlugin("com.versioneye" % "sbt-versioneye-plugin" % "0.2.0")

addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.3.7")