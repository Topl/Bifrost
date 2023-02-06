import sbt._

object Dependencies {

  val akkaVersion = "2.6.20"
  val akkaHttpVersion = "10.2.10"
  val circeVersion = "0.14.3"
  val kamonVersion = "2.5.12"
  val graalVersion = "21.3.4"
  val simulacrumVersion = "1.0.1"
  val catsCoreVersion = "2.9.0"
  val catsEffectVersion = "3.4.1"
  val fs2Version = "3.5-1c0be5c"
  val logback = "1.4.5"

  // Reference https://github.com/Topl/protobuf-specs/pull/30
  val protobufSpecs = "com.github.Topl" % "protobuf-specs" % "948dc20"

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

  val scalamock: Seq[ModuleID] = Seq(
    "org.scalamock" %% "scalamock" % "5.2.0" % "test"
  )

  val test: Seq[ModuleID] = Seq(
    "org.scalatest"    %% "scalatest"                     % "3.2.13" % "test",
    "com.ironcorelabs" %% "cats-scalatest"                % "3.1.1"  % "test",
    "org.typelevel"    %% "cats-effect-testing-scalatest" % "1.4.0"  % "test"
  ) ++ scalacheck ++ scalamock

  val mUnitTest: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit"                   % "0.7.29" % Test,
    "org.scalameta" %% "munit-scalacheck"        % "0.7.29" % Test,
    "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7"  % Test,
    "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4"  % Test
  ) ++ scalamock

  val it: Seq[ModuleID] = Seq(
    "org.scalatest"     %% "scalatest"           % "3.2.12"        % "it",
    "com.spotify"        % "docker-client"       % "8.16.0"        % "it",
    "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion     % "it",
    "com.typesafe.akka" %% "akka-http-testkit"   % akkaHttpVersion % "it"
  )

  def akka(name: String): ModuleID =
    "com.typesafe.akka" %% s"akka-$name" % akkaVersion

  def akkaHttp(name: String): ModuleID =
    "com.typesafe.akka" %% s"akka-$name" % akkaHttpVersion

  val allAkka: Seq[ModuleID] = Seq(
    "com.typesafe.akka" %% "akka-actor"               % akkaVersion,
    "com.typesafe.akka" %% "akka-actor-typed"         % akkaVersion,
    "com.typesafe.akka" %% "akka-stream"              % akkaVersion,
    "com.typesafe.akka" %% "akka-stream-typed"        % akkaVersion,
    "com.typesafe.akka" %% "akka-http"                % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-core"           % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-discovery"           % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j"               % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit"             % akkaVersion     % Test,
    "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion     % Test,
    "com.typesafe.akka" %% "akka-stream-testkit"      % akkaVersion     % Test,
    "com.typesafe.akka" %% "akka-http-testkit"        % akkaHttpVersion % Test
  )

  val network: Seq[ModuleID] = Seq(
    "org.bitlet"  % "weupnp"      % "0.1.4",
    "commons-net" % "commons-net" % "3.8.0"
  )

  val scalaCollectionCompat: Seq[ModuleID] = Seq(
    "org.scala-lang.modules" %% "scala-collection-compat" % "2.8.1"
  )

  val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion
  )

  val newType: Seq[ModuleID] = Seq(
    "io.estatico" %% "newtype" % "0.4.4"
  )

  val guava: Seq[ModuleID] = Seq(
    "com.google.guava" % "guava" % "31.1-jre"
  )

  val ficus: Seq[ModuleID] = Seq(
    "com.iheart" %% "ficus" % "1.5.2"
  )

  val shapeless = Seq(
    "com.chuusai" %% "shapeless" % "2.3.10"
  )

  val monitoring: Seq[ModuleID] = Seq(
    "io.kamon" %% "kamon-core"      % kamonVersion,
    "io.kamon" %% "kamon-cats-io-3" % kamonVersion,
    "io.kamon" %% "kamon-bundle"    % kamonVersion % Runtime,
    "io.kamon" %% "kamon-influxdb"  % kamonVersion % Runtime,
    "io.kamon" %% "kamon-zipkin"    % kamonVersion % Runtime
  )

  val graal: Seq[ModuleID] = Seq(
    "org.graalvm.sdk"     % "graal-sdk"   % graalVersion,
    "org.graalvm.js"      % "js"          % graalVersion,
    "org.graalvm.truffle" % "truffle-api" % graalVersion
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

  val mongoDb: Seq[ModuleID] =
    Seq(
      "org.mongodb.scala" %% "mongo-scala-driver" % "4.7.2"
    )

  val levelDb: Seq[ModuleID] = Seq(
    "org.ethereum"     % "leveldbjni-all" % "1.18.3",
    "org.iq80.leveldb" % "leveldb"        % "0.12"
  )

  val scodec = Seq(
    "org.scodec" %% "scodec-core" % "1.11.10",
    "org.scodec" %% "scodec-bits" % "1.1.34",
    "org.scodec" %% "scodec-cats" % "1.2.0"
  )

  val scodecCats: Seq[ModuleID] = Seq(
    "org.scodec" %% "scodec-cats" % "1.1.0"
  )

  val fleam: Seq[ModuleID] = Seq(
    "com.nike.fleam" %% "fleam" % "7.0.0"
  )

  val mainargs = Seq(
    "com.lihaoyi" %% "mainargs" % "0.3.0"
  )

  val monocle: Seq[ModuleID] = Seq(
    "com.github.julien-truffaut" %% "monocle-core"  % "3.0.0-M6",
    "com.github.julien-truffaut" %% "monocle-macro" % "3.0.0-M6"
  )

  val fs2Core = "co.fs2"                   %% "fs2-core"             % fs2Version
  val fs2IO = "co.fs2"                     %% "fs2-io"               % fs2Version
  val fs2ReactiveStreams = "co.fs2"        %% "fs2-reactive-streams" % fs2Version
  val pureConfig = "com.github.pureconfig" %% "pureconfig"           % "0.17.1"
  val circeYaml = "io.circe"               %% "circe-yaml"           % "0.14.1"
  val kubernetes = "io.kubernetes"          % "client-java"          % "16.0.1"

  val bramblScCrypto =
    "com.github.Topl" % "BramblSc" % "2.0.3"

  val catsAll: Seq[ModuleID] = cats ++ catsEffect ++ Seq(catsSlf4j)
  val fs2All: Seq[ModuleID] = catsAll ++ Seq(fs2Core, fs2IO)

  val node: Seq[ModuleID] =
    Seq(
      catsSlf4j,
      akka("actor-typed"),
      fs2Core,
      fs2IO,
      pureConfig,
      circeYaml
    ) ++
    cats ++
    catsEffect ++
    mainargs ++
    logging ++
    monocle ++
    monitoring ++
    it

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
      "com.google.cloud" % "google-cloud-storage" % "2.14.0"
    )

  lazy val actor: Seq[sbt.ModuleID] = fs2All

  lazy val algebras: Seq[sbt.ModuleID] =
    circe ++
    Seq(protobufSpecs) ++
    test ++
    catsEffect.map(_ % Test) ++
    Seq(catsSlf4j % Test)

  lazy val common: Seq[ModuleID] =
    Seq(
      "org.typelevel" %% "simulacrum" % simulacrumVersion
    ) ++
    scalaCollectionCompat ++
    logging ++
    scodec ++
    circe ++
    simulacrum ++
    test ++
    mongoDb ++
    Seq(akka("actor-typed"))

  val commonApplication: Seq[ModuleID] =
    cats ++ catsEffect ++ mainargs ++ logging ++ monocle ++
    simulacrum ++ Seq(
      catsSlf4j,
      akka("actor-typed"),
      pureConfig,
      circeYaml
    )

  lazy val brambl: Seq[ModuleID] =
    test ++ scodec ++ simulacrum ++ Seq(akkaHttp("http2-support"))

  lazy val akkaHttpRpc: Seq[ModuleID] =
    Seq(
      "de.heikoseeberger" %% "akka-http-circe" % "1.39.2",
      "io.circe"          %% "circe-optics"    % "0.14.1"
    ) ++
    scalaCollectionCompat ++
    circe ++
    allAkka ++
    test

  lazy val toplRpc: Seq[ModuleID] =
    scalaCollectionCompat ++
    scodec ++
    circe ++
    test

  lazy val benchmarking: Seq[ModuleID] = Seq()

  lazy val crypto: Seq[ModuleID] =
    circe ++
    scodec ++
    externalCrypto ++
    cats ++
    test ++
    Seq(bramblScCrypto)

  lazy val eventTree: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.catsEffect

  lazy val catsAkka: Seq[ModuleID] =
    cats ++ catsEffect ++ logging ++
    Seq(akka("actor"), akka("actor-typed"), akka("stream")) ++
    Seq(fs2Core, fs2IO, fs2ReactiveStreams)

  // TODO remove BN-714, PR v2
  lazy val models: Seq[ModuleID] =
    cats ++ simulacrum ++ newType ++ scodec ++ Seq(protobufSpecs)

  lazy val consensus: Seq[ModuleID] =
    Dependencies.mUnitTest ++ externalCrypto ++ Seq(akka("actor-typed")) ++ catsEffect ++ logging ++ scalacache

  lazy val minting: Seq[ModuleID] =
    Dependencies.mUnitTest ++ Dependencies.test ++ Dependencies.catsEffect

  lazy val networking: Seq[ModuleID] =
    Dependencies.test ++ Dependencies.catsEffect ++ Seq(
      Dependencies.akka("stream"),
      Dependencies.akka("stream-testkit") % Test
    ) ++ fleam

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

  lazy val loadTesting: Seq[ModuleID] =
    Seq(
      "com.lihaoyi"    %% "mainargs" % "0.2.3",
      "com.nike.fleam" %% "fleam"    % "7.0.0"
    ) ++
    fleam ++
    allAkka ++
    circe ++
    mainargs

  lazy val scalaPb =
    "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"

  lazy val toplGrpc: Seq[ModuleID] =
    Seq(
      scalaPb,
      "io.grpc" % "grpc-netty-shaded" % scalapb.compiler.Version.grpcJavaVersion
    ) ++
    cats ++
    catsEffect ++
    mUnitTest ++
    Seq(protobufSpecs)

  lazy val levelDbStore: Seq[ModuleID] =
    levelDb ++
    cats ++
    catsEffect ++
    mUnitTest ++
    Seq(fs2Core, fs2IO)

  lazy val orientDb: Seq[ModuleID] = {
    val orientDbVersion = "3.2.10"
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
  }

  lazy val genus: Seq[ModuleID] =
    Seq(
      "com.lightbend.akka"                    %% "akka-stream-alpakka-mongodb" % "3.0.4",
      "com.googlecode.concurrentlinkedhashmap" % "concurrentlinkedhashmap-lru" % "1.4.2",
      scalaPb
    ) ++
    allAkka ++
    circe ++
    cats ++
    mainargs ++
    orientDb ++
    ficus ++
    test

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
}
