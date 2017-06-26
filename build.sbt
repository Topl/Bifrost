name := "project-bifrost"

lazy val commonSettings = Seq(
  scalaVersion := "2.12.1",
  organization := "org.scorexfoundation",
  version := "2.0.0-M5-SNAPSHOT"
)

scalaVersion := "2.12.1"
organization := "org.scorexfoundation"
version := "2.0.0-M5-SNAPSHOT"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

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
  "ch.qos.logback" % "logback-core" % "1.+"
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



//todo: is it needed?
libraryDependencies += "org.atnos" %% "eff-cats" % "2.0.+"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.2",
  "org.scalacheck" %% "scalacheck" % "1.13.+"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "org.scorexfoundation" %% "iodb" % "0.3.+",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.17" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.7",
  "net.databinder.dispatch" %% "dispatch-core" % "+" % "test"
)

// https://mvnrepository.com/artifact/org.apache.httpcomponents/httpclient
libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.3"
// https://mvnrepository.com/artifact/org.apache.httpcomponents/httpasyncclient
libraryDependencies += "org.apache.httpcomponents" % "httpasyncclient" % "4.1.3"
// https://mvnrepository.com/artifact/org.apache.commons/commons-pool2
libraryDependencies += "org.apache.commons" % "commons-pool2" % "2.4.2"


libraryDependencies  ++= Seq(
  // Last snapshot
  "org.scalanlp" %% "breeze" % "latest.integration",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.13",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code.
  "org.scalanlp" %% "breeze-viz" % "0.13"
)

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq(
  "-server"
)

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

//publishing settings

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

fork := true

pomIncludeRepository := { _ => false }

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage := Some(url("https://github.com/Topl/Project-Bifrost"))

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

