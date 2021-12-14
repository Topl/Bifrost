import sbt._

object Dependencies {

  val akkaVersion = "2.6.16"
  val akkaHttpVersion = "10.2.6"
  val circeVersion = "0.14.1"
  val kamonVersion = "2.2.3"
  val graalVersion = "21.1.0"
  val simulacrumVersion = "1.0.1"

  val logging = Seq(
    "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.4",
    "ch.qos.logback"              % "logback-classic" % "1.2.8",
    "ch.qos.logback"              % "logback-core"    % "1.2.8",
    "org.slf4j"                   % "slf4j-api"       % "1.7.32"
  )

  val test = Seq(
    "org.scalatest"      %% "scalatest"         % "3.2.9"   % "test",
    "org.scalactic"      %% "scalactic"         % "3.2.9"   % "test",
    "org.scalacheck"     %% "scalacheck"        % "1.15.4"  % "test",
    "org.scalatestplus"  %% "scalacheck-1-14"   % "3.2.2.0" % "test",
    "com.spotify"         % "docker-client"     % "8.16.0"  % "test",
    "org.asynchttpclient" % "async-http-client" % "2.12.3"  % "test",
    "org.scalamock"      %% "scalamock"         % "5.1.0"   % "test",
    "com.ironcorelabs"   %% "cats-scalatest"    % "3.1.1"   % "test"
  )

  val it = Seq(
    "org.scalatest"     %% "scalatest"           % "3.2.6"         % "it",
    "com.spotify"        % "docker-client"       % "8.16.0"        % "it",
    "com.typesafe.akka" %% "akka-stream-testkit" % akkaVersion     % "it",
    "com.typesafe.akka" %% "akka-http-testkit"   % akkaHttpVersion % "it"
  )

  val akka = Seq(
    "com.typesafe.akka" %% "akka-actor"               % akkaVersion,
    "com.typesafe.akka" %% "akka-actor-typed"         % akkaVersion,
    "com.typesafe.akka" %% "akka-stream"              % akkaVersion,
    "com.typesafe.akka" %% "akka-stream-typed"        % akkaVersion,
    "com.typesafe.akka" %% "akka-http"                % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-core"           % akkaHttpVersion,
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
    "io.circe" %% "circe-core"   % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )

  val misc = Seq(
    "com.chuusai"     %% "shapeless" % "2.3.7",
    "com.iheart"      %% "ficus"     % "1.5.1",
    "com.google.guava" % "guava"     % "30.1.1-jre",
    "io.estatico"     %% "newtype"   % "0.4.4"
  )

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

  val node: Seq[ModuleID] = {
    Seq(
      "com.typesafe.akka"          %% "akka-cluster"       % akkaVersion,
      "com.typesafe.akka"          %% "akka-remote"        % akkaVersion,
      "com.typesafe"                % "config"             % "1.4.1",
      "com.lihaoyi"                %% "mainargs"           % "0.2.1",
      "net.jpountz.lz4"             % "lz4"                % "1.3.0",
      "com.github.julien-truffaut" %% "monocle-core"       % "3.0.0-M6",
      "com.github.julien-truffaut" %% "monocle-macro"      % "3.0.0-M6",
      "org.ethereum"                % "leveldbjni-all"     % "1.18.3",
      "org.iq80.leveldb"            % "leveldb"            % "0.12",
      "org.mongodb.scala"          %% "mongo-scala-driver" % "4.3.2"
    ) ++
    logging ++
    test ++
    it ++
    akka ++
    network ++
    circe ++
    misc ++
    monitoring
  }

  lazy val common: Seq[ModuleID] = {
    Seq(
      "org.typelevel"          %% "simulacrum"              % simulacrumVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0",
      "org.scodec"             %% "scodec-bits"             % "1.1.28"
    ) ++
    logging ++
    circe ++
    test ++
    akka
  }

  lazy val chainProgram: Seq[ModuleID] =
    Seq(
      "io.circe"               %% "circe-core"              % circeVersion,
      "io.circe"               %% "circe-parser"            % circeVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0"
    ) ++
    test ++
    graal

  lazy val brambl: Seq[ModuleID] =
    test

  lazy val akkaHttpRpc: Seq[ModuleID] =
    Seq(
      "de.heikoseeberger"      %% "akka-http-circe"         % "1.37.0",
      "io.circe"               %% "circe-optics"            % circeVersion,
      "io.circe"               %% "circe-generic"           % circeVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0"
    ) ++
    circe ++
    akka ++
    test

  lazy val toplRpc: Seq[ModuleID] =
    Seq(
      "io.circe"               %% "circe-generic"           % circeVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % "2.5.0"
    ) ++
    circe ++
    test

  lazy val gjallarhorn: Seq[ModuleID] =
    Seq(
      "com.typesafe.akka"     %% "akka-cluster" % akkaVersion,
      "com.typesafe.akka"     %% "akka-remote"  % akkaVersion,
      "com.github.pureconfig" %% "pureconfig"   % "0.16.0"
    ) ++
    akka ++
    test ++
    circe ++
    logging ++
    misc ++
    it

  lazy val benchmarking: Seq[ModuleID] = Seq()

  lazy val crypto: Seq[ModuleID] =
    Seq(
      "org.typelevel"     %% "simulacrum"      % simulacrumVersion,
      "org.typelevel"     %% "cats-core"       % "2.6.1",
      "org.bouncycastle"   % "bcprov-jdk15on"  % "1.69",
      "org.whispersystems" % "curve25519-java" % "0.5.0"
    ) ++
    misc ++
    test

  lazy val tools: Seq[ModuleID] =
    Seq(
      "org.mongodb.scala" %% "mongo-scala-driver" % "4.3.2"
    )

  lazy val loadTesting: Seq[ModuleID] = {
    Seq(
      "com.lihaoyi"    %% "mainargs" % "0.2.1",
      "com.nike.fleam" %% "fleam"    % "7.0.0"
    ) ++
    akka ++
    circe
  }

}
