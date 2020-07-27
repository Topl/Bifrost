import sbt.Keys.organization
import sbtassembly.MergeStrategy

name := "bifrost"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.11",
  organization := "co.topl",
  version := "1.1.0"
)

scalaVersion := "2.12.11"
organization := "co.topl"
version := "1.1.0"

mainClass in assembly := Some("bifrost.BifrostApp")
test in assembly := {}

// The Typesafe repository
resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

val akkaVersion = "2.5.30"
val akkaHttpVersion = "10.1.11"
val circeVersion = "0.13.0"

val akkaDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion % Test,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test,
)

val networkDependencies = Seq(
  "org.bitlet" % "weupnp" % "0.1.4",
  "commons-net" % "commons-net" % "3.6"
)

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-literal" % circeVersion
)

val loggingDependencies = Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "ch.qos.logback" % "logback-core" % "1.2.3",
  "org.slf4j" % "slf4j-api" % "1.7.25"
)

val testingDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.0.+" % Test,
  "org.scalatest" %% "scalatest" % "3.0.+" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.+" % Test,
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scorexfoundation" %% "scrypto" % "1.2.3",
  "com.google.guava" % "guava" % "19.0"
) ++ akkaDependencies ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.54",
  "org.whispersystems" % "curve25519-java" % "0.4.1",
)

// monitoring dependencies
libraryDependencies ++= Seq(
  "io.kamon" %% "kamon-bundle" % "2.0.5",
  "io.kamon" %% "kamon-core" % "2.1.0",
  "io.kamon" %% "kamon-influxdb" % "2.1.0",
  "io.kamon" %% "kamon-zipkin" % "2.1.0",
  //"io.kamon" %% "kamon-apm-reporter" % "2.1.0",
  //"de.aktey.akka.visualmailbox" %% "collector" % "1.1.0"
)

// https://mvnrepository.com/artifact/org.graalvm.sdk/graal-sdk
libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % "19.2.0"

// https://mvnrepository.com/artifact/org.graalvm.js/js
libraryDependencies += "org.graalvm.js" % "js" % "19.2.0"

// https://mvnrepository.com/artifact/org.graalvm.truffle/truffle-api
libraryDependencies += "org.graalvm.truffle" % "truffle-api" % "19.2.1"


libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "1.0",
  "com.google.protobuf" % "protobuf-java" % "3.5.1",
  "com.thesamet.scalapb" %% "lenses" % "0.7.0",
  "com.typesafe" % "config" % "1.3.3",
)

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq(
  "-Xbootclasspath/a:ValkyrieInstrument-1.0.jar",
  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-XX:+UseG1GC",
  "-XX:+UseNUMA",
  "-XX:+AlwaysPreTouch",
  "-XX:+PerfDisableSharedMem",
  "-XX:+ParallelRefProcEnabled",
  "-XX:+UseStringDeduplication",
  "-XX:+ExitOnOutOfMemoryError",
  "-Xss64m"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")

//publishing settings

publishMavenStyle := true

publishArtifact in Test := false

parallelExecution in Test := false

logBuffered in Test := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDG")

Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat

Compile / run / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat

Test / fork := false

Compile / run / fork := true

pomIncludeRepository := { _ => false }

homepage := Some(url("https://github.com/Topl/Bifrost"))

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

assemblyMergeStrategy in assembly ~= { old: ((String) => MergeStrategy) => {
    case ps if ps.endsWith(".SF")      => MergeStrategy.discard
    case ps if ps.endsWith(".DSA")     => MergeStrategy.discard
    case ps if ps.endsWith(".RSA")     => MergeStrategy.discard
    case ps if ps.endsWith(".xml")     => MergeStrategy.first
    // https://github.com/sbt/sbt-assembly/issues/370
    case PathList("module-info.class") => MergeStrategy.discard
    case PathList("module-info.java")  => MergeStrategy.discard
    case "META-INF/truffle/instrument" => MergeStrategy.concat
    case "META-INF/truffle/language"   => MergeStrategy.rename
    case x => old(x)
  }
}

connectInput in run := true
outputStrategy := Some(StdoutOutput)

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

PB.pythonExe := "C:\\Python27\\python.exe"

connectInput in run := true
outputStrategy := Some(StdoutOutput)

lazy val bifrost = Project(id = "bifrost", base = file("."))
  .settings(commonSettings: _*)

lazy val benchmarking = Project(id = "benchmark", base = file("benchmark"))
  .settings(commonSettings: _*)
  .dependsOn(bifrost % "compile->compile;test->test")
  .enablePlugins(JmhPlugin)
  .disablePlugins(sbtassembly.AssemblyPlugin)
