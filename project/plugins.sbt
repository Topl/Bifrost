// Comment to get more information during initialization
logLevel := Level.Error

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")

addSbtPlugin("com.github.tkawachi" % "sbt-lock" % "0.6.2")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")

//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.13")

libraryDependencies += "com.typesafe" % "config" % "1.3.0"

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.26")