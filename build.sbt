import sbt.Keys.organization
import sbtassembly.MergeStrategy

name := "project-bifrost"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.1",
  organization := "co.topl",
  version := "0.1.0-alpha"
)

scalaVersion := "2.12.1"
organization := "co.topl"
version := "0.1.0-alpha"

mainClass in assembly := Some("bifrost.console.BifrostConsole")

val circeVersion = "0.7+"

val networkDependencies = Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.17",
  "org.bitlet" % "weupnp" % "0.1.+",
  "commons-net" % "commons-net" % "3.+"
)

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
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
  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.+",
  "org.consensusresearch" %% "scrypto" % "1.2.+",
  "io.circe" %% "circe-optics" % circeVersion
) ++ networkDependencies ++ apiDependencies ++ loggingDependencies ++ testingDependencies

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.+",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.17" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.7",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test",
  "org.bouncycastle" % "bcprov-jdk15on" % "1.54"
)

val consoleDependencies = Seq(
  // https://mvnrepository.com/artifact/org.apache.httpcomponents/httpclient
  "org.apache.httpcomponents" % "httpclient" % "4.5.3",
  // https://mvnrepository.com/artifact/org.apache.httpcomponents/httpasyncclient
  "org.apache.httpcomponents" % "httpasyncclient" % "4.1.3",
  // https://mvnrepository.com/artifact/org.apache.commons/commons-pool2
  "org.apache.commons" % "commons-pool2" % "2.4.2"
)

libraryDependencies ++= consoleDependencies


libraryDependencies  ++= Seq(
  // Last snapshot
  "org.scalanlp" %% "breeze" % "latest.integration"
)

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq(
  "-server"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

//publishing settings

publishMavenStyle := true

publishArtifact in Test := false

fork := true

pomIncludeRepository := { _ => false }

homepage := Some(url("https://github.com/Topl/Project-Bifrost"))

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

assemblyMergeStrategy in assembly ~= { old: ((String) => MergeStrategy) => {
    case ps if ps.endsWith(".SF") => MergeStrategy.discard
    case ps if ps.endsWith(".DSA") => MergeStrategy.discard
    case ps if ps.endsWith(".RSA") => MergeStrategy.discard
    case x => old(x)
  }
}

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

PB.pythonExe := "C:\\Python27\\python.exe"
