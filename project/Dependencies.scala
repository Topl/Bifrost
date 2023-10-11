import sbt._

object Dependencies {

  val circeVersion = "0.14.6"
  val kamonVersion = "2.6.4"
  val simulacrumVersion = "1.0.1"
  val catsCoreVersion = "2.10.0"
  val catsEffectVersion = "3.5.2"
  val fs2Version = "3.9.2"
  val logback = "1.4.11"
  val orientDbVersion = "3.2.23"
  val ioGrpcVersion = "1.58.0"
  val http4sVersion = "0.23.23"
  val protobufSpecsVersion = "2.0.0-alpha5" // scala-steward:off
  val bramblScVersion = "2.0.0-alpha6" // scala-steward:off

  val catsSlf4j =
    "org.typelevel" %% "log4cats-slf4j" % "2.6.0"

  val logging: Seq[ModuleID] = Seq(
    "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.5",
    "ch.qos.logback"              % "logback-classic" % logback,
    "ch.qos.logback"              % "logback-core"    % logback,
    "org.slf4j"                   % "slf4j-api"       % "2.0.9",
    catsSlf4j
  )

  val scalamockBase = "org.scalamock" %% "scalamock" % "5.2.0"
  val scalamock = scalamockBase        % Test

  private val mUnitTestBase: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit"                   % "0.7.29",
    "org.scalameta" %% "munit-scalacheck"        % "0.7.29",
    "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7",
    "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4",
    scalamockBase
  )

  val mUnitTest: Seq[ModuleID] = mUnitTestBase.map(_ % Test)

  val dockerClient = "com.spotify" % "docker-client" % "8.16.0"

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
    "org.bouncycastle" % "bcprov-jdk18on" % "1.76"
  )

  val levelDb: Seq[ModuleID] = Seq(
    "io.github.tronprotocol" % "leveldbjni-all" % "1.23.2",
    "org.iq80.leveldb"       % "leveldb"        % "0.12"
  )

  val scodec = Seq(
    "org.scodec" %% "scodec-core" % "1.11.10",
    "org.scodec" %% "scodec-bits" % "1.1.37",
    "org.scodec" %% "scodec-cats" % "1.2.0"
  )

  val mainargs = Seq(
    "com.lihaoyi" %% "mainargs" % "0.5.4"
  )

  val fastparse = "com.lihaoyi" %% "fastparse" % "3.0.2"

  val monocle: Seq[ModuleID] = Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % "3.0.0-M6",
    "com.github.julien-truffaut" %% "monocle-macro" % "3.0.0-M6"
  )

  val fs2Core = "co.fs2"                   %% "fs2-core"             % fs2Version
  val fs2IO = "co.fs2"                     %% "fs2-io"               % fs2Version
  val fs2ReactiveStreams = "co.fs2"        %% "fs2-reactive-streams" % fs2Version
  val pureConfig = "com.github.pureconfig" %% "pureconfig"           % "0.17.4"
  val circeYaml = "io.circe"               %% "circe-yaml"           % "0.15.0-RC1"
  val kubernetes = "io.kubernetes"          % "client-java"          % "18.0.1"

  val http4s = Seq(
    "org.http4s" %% "http4s-ember-client" % http4sVersion,
    "org.http4s" %% "http4s-dsl"          % http4sVersion
  )

  val http4sServer = http4s ++ Seq(
    "org.http4s" %% "http4s-ember-server" % http4sVersion
  )

  val bramblScCrypto = "co.topl" %% "crypto"     % bramblScVersion
  val bramblScSdk = "co.topl"    %% "brambl-sdk" % bramblScVersion
  val quivr4s = "co.topl"        %% "quivr4s"    % bramblScVersion

  val protobufSpecs: Seq[ModuleID] = Seq(
    "co.topl" %% "protobuf-fs2" % protobufSpecsVersion
  )

  // For NTP-UDP
  val commonsNet = "commons-net" % "commons-net" % "3.9.0"

  val catsAll: Seq[ModuleID] = cats ++ catsEffect ++ Seq(catsSlf4j)
  val fs2All: Seq[ModuleID] = catsAll ++ Seq(fs2Core, fs2IO)

  val grpcServices = "io.grpc" % "grpc-services" % ioGrpcVersion

  val node: Seq[ModuleID] =
    Seq(
      catsSlf4j,
      fs2Core,
      fs2IO
    ) ++
    cats ++
    catsEffect ++
    mainargs ++
    logging ++
    monocle ++
    monitoring ++
    mUnitTestBase ++
    Seq(grpcServices) ++
    http4s

  val nodeIt =
    http4sServer.map(_ % Test)

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
      "com.google.cloud" % "google-cloud-storage" % "2.27.1"
    )

  lazy val actor: Seq[sbt.ModuleID] = fs2All

  lazy val algebras: Seq[sbt.ModuleID] =
    circe ++
    protobufSpecs ++
    munitScalamock ++
    catsEffect.map(_ % Test) ++
    Seq(catsSlf4j % Test)

  val commonApplication: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ monocle ++
    simulacrum ++ http4s ++ Seq(
      catsSlf4j,
      pureConfig,
      circeYaml
    )

  lazy val crypto: Seq[ModuleID] =
    scodec ++
    externalCrypto ++
    cats ++
    mUnitTest ++
    Seq(bramblScCrypto, bramblScCrypto.classifier("tests") % Test) ++
    circe.map(_ % Test)

  lazy val eventTree: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val catsUtils: Seq[ModuleID] =
    cats ++ catsEffect ++ logging ++ Seq(fs2Core, fs2IO, fs2ReactiveStreams)

  lazy val models: Seq[ModuleID] =
    cats ++ simulacrum ++ newType ++ scodec ++ protobufSpecs ++
    Seq(bramblScSdk, bramblScSdk.classifier("tests") % Test) ++
    Seq(quivr4s, quivr4s.classifier("tests") % Test)

  lazy val consensus: Seq[ModuleID] =
    Dependencies.mUnitTest ++ externalCrypto ++ catsEffect ++ logging ++ scalacache

  lazy val minting: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val networking: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val transactionGenerator: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ Seq(Dependencies.fs2Core)

  lazy val ledger: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ Dependencies.protobufSpecs ++ scalacache ++
    Seq(Dependencies.bramblScSdk, Dependencies.bramblScSdk.classifier("tests") % Test)

  lazy val blockchain: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect ++ logging ++ Seq(fs2Core)

  lazy val commonInterpreters: Seq[sbt.ModuleID] =
    mUnitTest ++
    cats ++
    catsEffect ++
    scalacache ++
    Seq(
      commonsNet,
      catsSlf4j % Test
    )

  lazy val byteCodecs: Seq[sbt.ModuleID] =
    munitScalamock ++
    simulacrum ++
    scodec ++
    cats

  lazy val toplGrpc: Seq[ModuleID] =
    cats ++
    catsEffect ++
    mUnitTest ++
    protobufSpecs ++
    Seq(
      "io.grpc" % "grpc-netty-shaded" % ioGrpcVersion,
      grpcServices
    )

  lazy val levelDbStore: Seq[ModuleID] =
    levelDb ++
    cats ++
    catsEffect ++
    mUnitTest ++
    Seq(fs2Core, fs2IO)

  lazy val orientDb: Seq[ModuleID] =
    Seq(
      "com.orientechnologies" % "orientdb-core"   % orientDbVersion,
      "com.orientechnologies" % "orientdb-server" % orientDbVersion,
      "com.orientechnologies" % "orientdb-client" % orientDbVersion,
      "com.orientechnologies" % "orientdb-tools"  % orientDbVersion,
      "com.orientechnologies" % "orientdb-graphdb" % orientDbVersion exclude ("commons-beanutils", "commons-beanutils") exclude ("commons-beanutils", "commons-beanutils-core"),
      "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.4.2",
      "org.lz4"                                % "lz4-java"                    % "1.8.0"
      // Add jna
    )

  lazy val genusLibrary: Seq[ModuleID] =
    logging ++
    orientDb ++
    mUnitTest ++
    simulacrum

  lazy val munitScalamock: Seq[sbt.ModuleID] =
    mUnitTest

  lazy val byzantineIt: Seq[ModuleID] =
    (mUnitTestBase :+ dockerClient :+ fastparse).map(_ % Test)
}
