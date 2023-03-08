import sbt._

object Dependencies {

  val akkaVersion = "2.6.20"
  val circeVersion = "0.14.5"
  val kamonVersion = "2.5.12"
  val simulacrumVersion = "1.0.1"
  val catsCoreVersion = "2.9.0"
  val catsEffectVersion = "3.4.8"
  val fs2Version = "3.6.1"
  val logback = "1.4.5"
  val orientDbVersion = "3.2.16"
  val protobufSpecsVersion = "c226e4c" // scala-steward:off

  val catsSlf4j =
    "org.typelevel" %% "log4cats-slf4j" % "2.5.0"

  val logging: Seq[ModuleID] = Seq(
    "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.5",
    "ch.qos.logback"              % "logback-classic" % logback,
    "ch.qos.logback"              % "logback-core"    % logback,
    "org.slf4j"                   % "slf4j-api"       % "2.0.4",
    catsSlf4j
  )

  val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck"    %% "scalacheck"      % "1.16.0"  % "test",
    "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test"
  )

  val scalamockBase = "org.scalamock" %% "scalamock" % "5.2.0"
  val scalamock = scalamockBase        % Test

  val test: Seq[ModuleID] = Seq(
    "org.scalatest"    %% "scalatest"                     % "3.2.13" % "test",
    "com.ironcorelabs" %% "cats-scalatest"                % "3.1.1"  % "test",
    "org.typelevel"    %% "cats-effect-testing-scalatest" % "1.4.0"  % "test",
    scalamock
  ) ++ scalacheck

  private val mUnitTestBase: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit"                   % "0.7.29",
    "org.scalameta" %% "munit-scalacheck"        % "0.7.29",
    "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7",
    "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4",
    scalamockBase
  )

  val mUnitTest: Seq[ModuleID] = mUnitTestBase.map(_ % Test)

  val dockerClient = "com.spotify" % "docker-client" % "8.16.0"

  def akka(name: String): ModuleID =
    "com.typesafe.akka" %% s"akka-$name" % akkaVersion

  val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion
  )

  val newType: Seq[ModuleID] = Seq(
    "io.estatico" %% "newtype" % "0.4.4"
  )

  val monitoring: Seq[ModuleID] = Seq(
    "io.kamon" %% "kamon-core"      % kamonVersion,
    "io.kamon" %% "kamon-cats-io-3" % kamonVersion
  )

  val cats: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-core" % catsCoreVersion,
    "org.typelevel" %% "mouse"     % "1.2.1"
  )

  val catsEffect: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-effect" % catsEffectVersion
  )

  val scalacache: Seq[ModuleID] = Seq(
    "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M6"
  )

  val simulacrum: Seq[ModuleID] = Seq(
    "org.typelevel" %% "simulacrum" % simulacrumVersion
  )

  val externalCrypto: Seq[ModuleID] = Seq(
    "org.bouncycastle" % "bcprov-jdk18on" % "1.72"
  )

  val levelDb: Seq[ModuleID] = Seq(
    "org.ethereum"     % "leveldbjni-all" % "1.18.3",
    "org.iq80.leveldb" % "leveldb"        % "0.12"
  )

  val scodec = Seq(
    "org.scodec" %% "scodec-core" % "1.11.10",
    "org.scodec" %% "scodec-bits" % "1.1.37",
    "org.scodec" %% "scodec-cats" % "1.2.0"
  )

  val mainargs = Seq(
    "com.lihaoyi" %% "mainargs" % "0.4.0"
  )

  val monocle: Seq[ModuleID] = Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % "3.0.0-M6",
    "com.github.julien-truffaut" %% "monocle-macro" % "3.0.0-M6"
  )

  val fs2Core = "co.fs2"                   %% "fs2-core"             % fs2Version
  val fs2IO = "co.fs2"                     %% "fs2-io"               % fs2Version
  val fs2ReactiveStreams = "co.fs2"        %% "fs2-reactive-streams" % fs2Version
  val pureConfig = "com.github.pureconfig" %% "pureconfig"           % "0.17.2"
  val circeYaml = "io.circe"               %% "circe-yaml"           % "0.14.2"
  val kubernetes = "io.kubernetes"          % "client-java"          % "18.0.0"

  val bramblScCrypto = "com.github.Topl"        % "BramblSc"   % "v2.0.3"
  val bramblScSdk = "com.github.Topl.bramblsc" %% "brambl-sdk" % "652cdaa7a7" // scala-steward:off
  val quivr4s = "com.github.Topl"               % "quivr4s"    % "3bcc730" // scala-steward:off

  val protobufSpecs: Seq[ModuleID] = Seq(
    "com.github.Topl" % "protobuf-specs" % protobufSpecsVersion
  )

  val catsAll: Seq[ModuleID] = cats ++ catsEffect ++ Seq(catsSlf4j)
  val fs2All: Seq[ModuleID] = catsAll ++ Seq(fs2Core, fs2IO)

  val node: Seq[ModuleID] =
    Seq(
      catsSlf4j,
      akka("actor-typed"),
      fs2Core,
      fs2IO
    ) ++
    cats ++
    catsEffect ++
    mainargs ++
    logging ++
    monocle ++
    monitoring ++
    mUnitTestBase.map(_ % IntegrationTest)

  val networkDelayer: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ Seq(
      catsSlf4j,
      fs2Core,
      fs2IO,
      pureConfig
    )

  val testnetSimulationOrchestator: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ Seq(
      catsSlf4j,
      fs2Core,
      fs2IO,
      pureConfig,
      kubernetes,
      "com.google.cloud" % "google-cloud-storage" % "2.20.1"
    )

  lazy val actor: Seq[sbt.ModuleID] = fs2All

  lazy val algebras: Seq[sbt.ModuleID] =
    circe ++
    protobufSpecs ++
    test ++
    catsEffect.map(_ % Test) ++
    Seq(catsSlf4j % Test)

  val commonApplication: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ monocle ++
    simulacrum ++ Seq(
      catsSlf4j,
      akka("actor-typed"),
      pureConfig,
      circeYaml
    )

  lazy val crypto: Seq[ModuleID] =
    scodec ++
    externalCrypto ++
    cats ++
    test ++
    Seq(bramblScCrypto) ++
    circe.map(_ % Test)

  lazy val eventTree: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val catsAkka: Seq[ModuleID] =
    cats ++ catsEffect ++ logging ++
    Seq(akka("actor"), akka("actor-typed"), akka("stream")) ++
    Seq(fs2Core, fs2IO, fs2ReactiveStreams)

  lazy val models: Seq[ModuleID] =
    cats ++ simulacrum ++ newType ++ scodec ++ protobufSpecs ++
    Seq(bramblScSdk).map(_ classifier ("tests")).map(_ % Test) ++
    Seq(quivr4s).map(_ classifier ("tests")).map(_ % Test)

  lazy val consensus: Seq[ModuleID] =
    Dependencies.mUnitTest ++ externalCrypto ++ Seq(akka("actor-typed")) ++ catsEffect ++ logging ++ scalacache

  lazy val minting: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.test ++ Dependencies.catsEffect

  lazy val networking: Seq[ModuleID] =
    Dependencies.test ++ Dependencies.catsEffect ++ Seq(
      Dependencies.akka("stream"),
      Dependencies.akka("stream-testkit") % Test
    )

  lazy val transactionGenerator: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ Seq(Dependencies.fs2Core)

  lazy val ledger: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val blockchain: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ logging ++ Seq(
      akka("stream"),
      akka("stream-testkit") % Test
    ) ++ Seq(fs2Core)

  lazy val commonInterpreters: Seq[sbt.ModuleID] =
    mUnitTest ++
    Seq(
      catsSlf4j % "test"
    ) ++
    cats ++
    catsEffect ++
    scalacache

  lazy val byteCodecs: Seq[sbt.ModuleID] =
    test ++
    simulacrum ++
    scodec ++
    cats ++
    Seq(akka("actor"))

  lazy val toplGrpc: Seq[ModuleID] =
    cats ++
    catsEffect ++
    mUnitTest ++
    protobufSpecs ++
    Seq(
      "io.grpc" % "grpc-netty-shaded" % "1.53.0"
    )

  lazy val levelDbStore: Seq[ModuleID] =
    levelDb ++
    cats ++
    catsEffect ++
    mUnitTest ++
    Seq(fs2Core, fs2IO)

  lazy val orientDb: Seq[ModuleID] =
    Seq(
      "com.orientechnologies"                  % "orientdb-core"               % orientDbVersion,
      "com.orientechnologies"                  % "orientdb-server"             % orientDbVersion,
      "com.orientechnologies"                  % "orientdb-client"             % orientDbVersion,
      "com.orientechnologies"                  % "orientdb-tools"              % orientDbVersion,
      "com.orientechnologies"                  % "orientdb-graphdb"            % orientDbVersion,
      "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.4.2",
      "org.lz4"                                % "lz4-java"                    % "1.8.0"
      // Add jna
    )

  lazy val genusServer: Seq[ModuleID] =
    cats ++
    catsEffect ++
    mainargs ++
    logging ++
    monocle ++
    Seq(
      catsSlf4j
    ) ++
    mUnitTest

  lazy val genusLibrary: Seq[ModuleID] =
    logging ++
    orientDb ++
    mUnitTest ++
    simulacrum

  lazy val munitScalamock: Seq[sbt.ModuleID] =
    mUnitTest

  lazy val byzantineTests: Seq[ModuleID] =
    (mUnitTestBase :+ dockerClient).map(_ % IntegrationTest)
}
