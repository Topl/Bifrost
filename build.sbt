import sbt.Keys.organization
import sbtassembly.MergeStrategy
import com.github.tkawachi.sbtlock._

name := "bifrost"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.10",
  organization := "co.topl",
  version := "1.1.0"
)

scalaVersion := "2.12.10"
organization := "co.topl"
version := "1.1.0"

mainClass in assembly := Some("bifrost.BifrostApp")
test in assembly := {}

val akkaVersion = "2.5.26"
val akkaHttpVersion = "10.1.11"
val circeVersion = "0.11.1"

val akkaDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
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
  "io.circe" %% "circe-literal" % circeVersion,
  "io.swagger" %% "swagger-scala-module" % "1.0.3",
  "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.9.2",
)

val loggingDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "ch.qos.logback" % "logback-core" % "1.2.3",
  "com.typesafe.akka" % "akka-slf4j_2.12" % "2.4.17"
)

val testingDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.0.+" % Test,
  "org.scalatest" %% "scalatest" % "3.0.+" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.+" % Test,
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scorexfoundation" %% "scrypto" % "1.2.3",
  "io.circe" %% "circe-optics" % "0.13.0"
) ++ akkaDependencies ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.54",
  "io.kamon" %% "kamon-bundle" % "2.0.6",
  "io.kamon" %% "kamon-influxdb" % "2.0.0",
  "io.kamon" %% "kamon-zipkin" % "2.0.1"
)

libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.2"
libraryDependencies += "com.thesamet.scalapb" %% "scalapb-json4s" % "0.7.0"


// https://mvnrepository.com/artifact/org.graalvm.sdk/graal-sdk
libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % "19.2.0"

// https://mvnrepository.com/artifact/org.graalvm.js/js
libraryDependencies += "org.graalvm.js" % "js" % "19.2.0"

// https://mvnrepository.com/artifact/org.graalvm.truffle/truffle-api
libraryDependencies += "org.graalvm.truffle" % "truffle-api" % "19.2.0"


libraryDependencies  ++= Seq(
  // Last snapshot
  "org.scalanlp" %% "breeze" % "latest.integration"
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

lazy val bifrost = Project(id = "project-bifrost", base = file("."))
  .settings(commonSettings: _*)