// Comment to get more information during initialization
logLevel := Level.Error

// The Typesafe repository
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")

addSbtPlugin("com.github.tkawachi" % "sbt-lock" % "0.3.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")

libraryDependencies += "com.typesafe" % "config" % "1.3.0"

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
//sbt dependencyTree

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.26")