import sbt.Keys.organization
import sbtassembly.MergeStrategy

name := "bifrost"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.11",
  semanticdbEnabled := true, // enable SemanticDB for Scalafix
  semanticdbVersion := scalafixSemanticdb.revision, // use Scalafix compatible version
  organization := "co.topl",
  version := "1.1.0",
//  wartremoverErrors := Warts.unsafe // settings for wartremover
)

scalaVersion := "2.12.11"
organization := "co.topl"
version := "1.1.0"

mainClass in assembly := Some("bifrost.BifrostApp")
test in assembly := {}

// The Typesafe repository
resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

val akkaVersion = "2.5.31"
val akkaHttpVersion = "10.2.0"
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
  "commons-net" % "commons-net" % "3.7"
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
  "org.slf4j" % "slf4j-api" % "1.7.30"
)

val testingDependencies = Seq(
  "org.scalactic" %% "scalactic" % "3.2.2" % Test,
  "org.scalatest" %% "scalatest" % "3.2.2" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
  "com.spotify" % "docker-client" % "8.16.0" % Test,
  "org.asynchttpclient" % "async-http-client" % "2.7.0" % Test
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scorexfoundation" %% "scrypto" % "2.1.9",
  "com.google.guava" % "guava" % "29.0-jre",
  "com.iheart" %% "ficus" % "1.4.7",
  "org.rudogma" %% "supertagged" % "1.5",
  "com.joefkelley" %% "argyle" % "1.0.0",
) ++ akkaDependencies ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.4.0",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.66",
  "org.whispersystems" % "curve25519-java" % "0.5.0",
)

// monitoring dependencies
libraryDependencies ++= Seq(
  "io.kamon" %% "kamon-bundle" % "2.0.6",
  "io.kamon" %% "kamon-core" % "2.1.6",
  "io.kamon" %% "kamon-influxdb" % "2.1.6",
  "io.kamon" %% "kamon-zipkin" % "2.1.6",
  //"io.kamon" %% "kamon-apm-reporter" % "2.1.0",
  //"de.aktey.akka.visualmailbox" %% "collector" % "1.1.0"
)

// https://mvnrepository.com/artifact/org.graalvm.sdk/graal-sdk
libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % "19.2.1"

// https://mvnrepository.com/artifact/org.graalvm.js/js
libraryDependencies += "org.graalvm.js" % "js" % "19.2.1"

// https://mvnrepository.com/artifact/org.graalvm.truffle/truffle-api
libraryDependencies += "org.graalvm.truffle" % "truffle-api" % "19.3.3"


libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "1.1",
  "com.google.protobuf" % "protobuf-java" % "3.13.0",
  "com.thesamet.scalapb" %% "lenses" % "0.10.8",
  "com.typesafe" % "config" % "1.3.4",
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint:",
  "-Ywarn-unused:-implicits,-privates"
)

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

classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat

Test / fork := false

Compile / run / fork := true

pomIncludeRepository := { _ => false }

homepage := Some(url("https://github.com/Topl/Bifrost"))

assemblyJarName := s"bifrost-${version.value}.jar"

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

assemblyExcludedJars in assembly := {
  val cp = (fullClasspath in assembly).value
  cp filter { el ⇒
    (el.data.getName == "ValkyrieInstrument-1.0.jar")
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

lazy val it = Project(id = "it", base = file("it"))
  .settings(commonSettings: _*)
  .dependsOn(bifrost % "compile->compile;test->test")
  .disablePlugins(sbtassembly.AssemblyPlugin)