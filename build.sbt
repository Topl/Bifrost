import sbt.Keys.organization
import sbtassembly.MergeStrategy
import com.github.tkawachi.sbtlock._

name := "bifrost"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.7",
  organization := "co.topl",
  version := "1.1.0"
)

scalaVersion := "2.12.7"
organization := "co.topl"
version := "1.1.0"

mainClass in assembly := Some("bifrost.BifrostApp")

test in assembly := {}

//TODO Update iodb in sbt.lock
excludeDependencies in SbtLockKeys.lock := Seq(
  "org.scorexfoundation" %% "iodb"
)
dependencyOverrides += "org.scorexfoundation" %% "iodb" % "0.3.2"

val circeVersion = "0.7+"

val networkDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.19",
  "org.bitlet" % "weupnp" % "0.1.+",
  "commons-net" % "commons-net" % "3.+"
)

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.circe" %% "circe-literal" % circeVersion,
  "io.swagger" %% "swagger-scala-module" % "1.0.3",
  // "io.swagger" % "swagger-core" % "1.5.10",
  // "io.swagger" % "swagger-annotations" % "1.5.10",
  // "io.swagger" % "swagger-models" % "1.5.10",
  // "io.swagger" % "swagger-jaxrs" % "1.5.10",
  "com.github.swagger-akka-http" %% "swagger-akka-http" % "0.+",
  "com.typesafe.akka" %% "akka-http" % "10.+"
)

val loggingDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.+",
  "ch.qos.logback" % "logback-core" % "1.+",
  "com.typesafe.akka" % "akka-slf4j_2.12" % "2.4.17"
)

val testingDependencies = Seq(
  "com.typesafe.akka" %% "akka-testkit" % "2.4.17" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+",
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/Desktop/ValkyrieInstrument"


libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.+",
  "org.consensusresearch" %% "scrypto" % "1.2.+",
  "io.circe" %% "circe-optics" % circeVersion
) ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.17" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.7",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.54"
)

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.19"


libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.2"
libraryDependencies += "com.thesamet.scalapb" %% "scalapb-json4s" % "0.7.0"

val consoleDependencies = Seq(
  // https://mvnrepository.com/artifact/org.apache.httpcomponents/httpclient
  "org.apache.httpcomponents" % "httpclient" % "4.5.3",
  // https://mvnrepository.com/artifact/org.apache.httpcomponents/httpasyncclient
  "org.apache.httpcomponents" % "httpasyncclient" % "4.1.3",
  // https://mvnrepository.com/artifact/org.apache.commons/commons-pool2
  "org.apache.commons" % "commons-pool2" % "2.4.2"
)

// https://mvnrepository.com/artifact/org.graalvm.sdk/graal-sdk
libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % "19.2.0"

// https://mvnrepository.com/artifact/org.graalvm.js/js
libraryDependencies += "org.graalvm.js" % "js" % "19.2.0"

// https://mvnrepository.com/artifact/org.graalvm.truffle/truffle-api
libraryDependencies += "org.graalvm.truffle" % "truffle-api" % "19.2.0"

libraryDependencies ++= consoleDependencies


libraryDependencies  ++= Seq(
  // Last snapshot
  "org.scalanlp" %% "breeze" % "latest.integration"
)

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq(
  "-J-Dcom.sun.management.jmxremote",
  "-J-Xbootclasspath/a:ValkyrieInstrument-1.0.jar",
  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-J-XX:+UseG1GC",
  "-J-XX:+UseNUMA",
  "-J-XX:+AlwaysPreTouch",
  "-J-XX:+PerfDisableSharedMem",
  "-J-XX:+ParallelRefProcEnabled",
  "-J-XX:+UseStringDeduplication",
  "-J-XX:+ExitOnOutOfMemoryError"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")

//publishing settings

publishMavenStyle := true

publishArtifact in Test := false

parallelExecution in Test := false

logBuffered in Test := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDG")

fork := false

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

lazy val programModules = Project(id = "program-modules", base = file("program-modules"))
  .settings(commonSettings: _*)
  .enablePlugins(ScalaJSPlugin)
  .disablePlugins(sbtassembly.AssemblyPlugin)
