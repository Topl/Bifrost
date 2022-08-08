import sbt._

object Dependencies {

  val akkaVersion = "2.6.18"
  val akkaHttpVersion = "10.2.9"
  val circeVersion = "0.14.1"
  val kamonVersion = "2.5.0"
  val graalVersion = "21.1.0"
  val simulacrumVersion = "1.0.1"
  val catsCoreVersion = "2.6.1"
  val catsEffectVersion = "3.3.0"

  val catsSlf4j =
    "org.typelevel" %% "log4cats-slf4j" % "2.1.1"

  val logging = Seq(
    "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.4",
    "ch.qos.logback"              % "logback-classic" % "1.2.11",
    "ch.qos.logback"              % "logback-core"    % "1.2.11",
    "org.slf4j"                   % "slf4j-api"       % "1.7.36",
    catsSlf4j
  )

  val test = Seq(
    "org.scalatest"      %% "scalatest"                     % "3.2.11"  % "test",
    "org.scalactic"      %% "scalactic"                     % "3.2.11"  % "test",
    "org.scalacheck"     %% "scalacheck"                    % "1.15.4"  % "test",
    "org.scalatestplus"  %% "scalacheck-1-14"               % "3.2.2.0" % "test",
    "com.spotify"         % "docker-client"                 % "8.16.0"  % "test",
    "org.asynchttpclient" % "async-http-client"             % "2.12.3"  % "test",
    "org.scalamock"      %% "scalamock"                     % "5.2.0"   % "test",
    "com.ironcorelabs"   %% "cats-scalatest"                % "3.1.1"   % "test",
    "org.typelevel"      %% "cats-effect-testing-scalatest" % "1.3.0"   % "test"
  )

  val it = Seq(
    "org.scalatest"     %% "scalatest"           % "3.2.6"         % "it",
    "com.spotify"        % "docker-client"       % "8.16.0"        % "it",
    "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion     % "it",
    "com.typesafe.akka" %% "akka-http-testkit"   % akkaHttpVersion % "it"
  )

  def akka(name: String): ModuleID =
    "com.typesafe.akka" %% s"akka-$name" % akkaVersion

  val allAkka = Seq(
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

  val network = Seq(
    "org.bitlet"  % "weupnp"      % "0.1.4",
    "commons-net" % "commons-net" % "3.8.0"
  )

  val circe = Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion
  )

  val newType = Seq(
    "io.estatico" %% "newtype" % "0.4.4"
  )

  val guava = Seq(
    "com.google.guava" % "guava" % "31.0.1-jre"
  )

  val misc = Seq(
    "com.chuusai" %% "shapeless" % "2.3.9",
    "com.iheart"  %% "ficus"     % "1.5.2"
  ) ++ guava ++ newType

  val monitoring = Seq(
    "io.kamon" %% "kamon-core"     % kamonVersion,
    "io.kamon" %% "kamon-bundle"   % kamonVersion % Runtime,
    "io.kamon" %% "kamon-influxdb" % kamonVersion % Runtime,
    "io.kamon" %% "kamon-zipkin"   % kamonVersion % Runtime
  )

  val graal = Seq(
    "org.graalvm.sdk"     % "graal-sdk"   % graalVersion,
    "org.graalvm.js"      % "js"          % graalVersion,
    "org.graalvm.truffle" % "truffle-api" % graalVersion
  )

  val cats = Seq(
    "org.typelevel" %% "cats-core" % catsCoreVersion
  )

  val catsEffect = Seq(
    "org.typelevel" %% "cats-effect" % catsEffectVersion
  )

  val scalacache = Seq(
    "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M4"
  )

  val simulacrum = Seq(
    "org.typelevel" %% "simulacrum" % simulacrumVersion
  )

  val bouncyCastle = Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.69"
  )

  val levelDb = Seq(
    "org.ethereum"     % "leveldbjni-all" % "1.18.3",
    "org.iq80.leveldb" % "leveldb"        % "0.12"
  )

  val scodec = Seq(
    "org.scodec" %% "scodec-core" % "1.11.9"
  )

  val scodecBits = Seq(
    "org.scodec" %% "scodec-bits" % "1.1.27"
  )

  val fleam = Seq(
    "com.nike.fleam" %% "fleam" % "7.0.0"
  )

  val mainargs = Seq(
    "com.lihaoyi" %% "mainargs" % "0.2.2"
  )

  val node: Seq[ModuleID] =
    Seq(
      "com.typesafe.akka"          %% "akka-cluster"       % akkaVersion,
      "com.typesafe.akka"          %% "akka-remote"        % akkaVersion,
      "com.typesafe"                % "config"             % "1.4.2",
      "net.jpountz.lz4"             % "lz4"                % "1.3.0",
      "com.github.julien-truffaut" %% "monocle-core"       % "3.0.0-M6",
      "com.github.julien-truffaut" %% "monocle-macro"      % "3.0.0-M6",
      "org.mongodb.scala"          %% "mongo-scala-driver" % "4.5.0"
    ) ++
    levelDb ++
    logging ++
    test ++
    it ++
    allAkka ++
    network ++
    circe ++
    misc ++
    monitoring ++
    mainargs

  lazy val algebras =
    test ++
    catsEffect.map(_ % Test) ++
    Seq(catsSlf4j % Test)

  lazy val common: Seq[ModuleID] =
    Seq(
      "org.typelevel"          %% "simulacrum"              % simulacrumVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.6.0",
      "org.mongodb.scala"      %% "mongo-scala-driver"      % "4.3.2",
      "io.circe"               %% "circe-generic"           % circeVersion,
      "org.scodec"             %% "scodec-core"             % "1.11.8",
      "org.scodec"             %% "scodec-bits"             % "1.1.30"
    ) ++
    logging ++
    scodecBits ++
    circe ++
    simulacrum ++
    test ++
    tools ++
    Seq(akka("actor-typed"))

  lazy val chainProgram: Seq[ModuleID] =
    Seq(
      "io.circe"               %% "circe-core"              % circeVersion,
      "io.circe"               %% "circe-parser"            % circeVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.6.0"
    ) ++
    test ++
    graal

  lazy val brambl: Seq[ModuleID] =
    test ++ scodec ++ simulacrum

  lazy val akkaHttpRpc: Seq[ModuleID] =
    Seq(
      "de.heikoseeberger"      %% "akka-http-circe"         % "1.39.2",
      "io.circe"               %% "circe-optics"            % circeVersion,
      "io.circe"               %% "circe-generic"           % circeVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.6.0"
    ) ++
    circe ++
    allAkka ++
    test

  lazy val toplRpc: Seq[ModuleID] =
    Seq(
      "io.circe"               %% "circe-generic"           % circeVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.6.0"
    ) ++
    circe ++
    test

  lazy val gjallarhorn: Seq[ModuleID] =
    Seq(
      "com.typesafe.akka"     %% "akka-cluster" % akkaVersion,
      "com.typesafe.akka"     %% "akka-remote"  % akkaVersion,
      "com.github.pureconfig" %% "pureconfig"   % "0.16.0"
    ) ++
    allAkka ++
    test ++
    circe ++
    logging ++
    misc ++
    it

  lazy val benchmarking: Seq[ModuleID] = Seq()

  lazy val crypto: Seq[ModuleID] =
    Seq(
      "org.bouncycastle"   % "bcprov-jdk15on"  % "1.70",
      "org.whispersystems" % "curve25519-java" % "0.5.0"
    ) ++
    scodecBits ++
    misc ++
    circe ++
    bouncyCastle ++
    cats ++
    simulacrum ++
    cats ++
    test

  lazy val catsAkka: Seq[ModuleID] =
    cats ++ catsEffect ++ logging ++ Seq(akka("actor"), akka("actor-typed"), akka("stream"))

  lazy val models: Seq[ModuleID] =
    cats ++ simulacrum ++ newType ++ scodecBits

  lazy val consensus: Seq[ModuleID] =
    bouncyCastle ++ Seq(akka("actor-typed")) ++ catsEffect ++ logging ++ scalacache

  lazy val minting: Seq[ModuleID] =
    Dependencies.test ++ Dependencies.catsEffect ++ Seq(Dependencies.akka("stream"))

  lazy val networking: Seq[ModuleID] =
    Dependencies.test ++ Dependencies.catsEffect ++ Seq(
      Dependencies.akka("stream"),
      Dependencies.akka("stream-testkit") % Test
    ) ++ fleam

  lazy val demo: Seq[ModuleID] =
    Seq(akka("actor"), akka("actor-typed"), akka("stream")) ++ logging

  lazy val commonInterpreters =
    Dependencies.test ++
    Seq(
      akka("actor-typed"),
      akka("actor-testkit-typed") % Test,
      Dependencies.catsSlf4j      % "test"
    ) ++
    Dependencies.cats ++
    Dependencies.catsEffect ++
    Dependencies.scalacache

  lazy val tools: Seq[ModuleID] =
    Seq(
      "org.mongodb.scala" %% "mongo-scala-driver" % "4.5.0"
    )

  lazy val loadTesting: Seq[ModuleID] =
    Seq(
      "com.lihaoyi"    %% "mainargs" % "0.2.1",
      "com.nike.fleam" %% "fleam"    % "7.0.0"
    ) ++
    fleam ++
    allAkka ++
    circe ++
    mainargs

  lazy val genus: Seq[ModuleID] =
    Seq(
      "com.lightbend.akka"   %% "akka-stream-alpakka-mongodb" % "3.0.4",
      "com.thesamet.scalapb" %% "scalapb-runtime"             % scalapb.compiler.Version.scalapbVersion % "protobuf"
    ) ++
    allAkka ++
    circe ++
    cats ++
    mainargs ++
    misc ++
    test
}
