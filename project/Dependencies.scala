import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt.{Def, _}

object Dependencies {

  val akkaVersion = "2.6.14"
  val akkaHttpVersion = "10.2.7"
  val circeVersion = "0.14.1"
  val kamonVersion = "2.4.2"
  val graalVersion = "21.1.0"
  val simulacrumVersion = "1.0.1"

  val catsSlf4j =
    Def.setting("org.typelevel" %% "log4cats-slf4j" % "2.1.1")

  val logging =
    Def.setting(
      Seq(
        "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.4",
        "ch.qos.logback"              % "logback-classic" % "1.2.7",
        "ch.qos.logback"              % "logback-core"    % "1.2.7",
        "org.slf4j"                   % "slf4j-api"       % "1.7.32",
        catsSlf4j.value
      )
    )

  val test = Def.setting(
    Seq(
      "org.scalatest" %%% "scalatest"           % "3.2.10"            % "test",
      "org.scalactic" %%% "scalactic"           % "3.2.10"            % "test",
      "org.scalacheck" %%% "scalacheck"         % "1.15.4"            % "test",
      "org.scalatestplus" %%% "scalacheck-1-14" % "3.2.2.0"           % "test",
      "com.spotify"                             % "docker-client"     % "8.16.0" % "test",
      "org.asynchttpclient"                     % "async-http-client" % "2.12.3" % "test",
      "org.scalamock" %%% "scalamock"           % "5.1.0"             % "test",
      "com.ironcorelabs" %%% "cats-scalatest"   % "3.1.1"             % "test"
    )
  )

  val it = Def.setting(
    Seq(
      "org.scalatest" %%% "scalatest"               % "3.2.6"         % "it",
      "com.spotify"                                 % "docker-client" % "8.16.0" % "it",
      "com.typesafe.akka" %%% "akka-stream-testkit" % akkaVersion     % "it",
      "com.typesafe.akka" %%% "akka-http-testkit"   % akkaHttpVersion % "it"
    )
  )

  def akka(name: String): Def.Initialize[sbt.ModuleID] =
    Def.setting("com.typesafe.akka" %% s"akka-$name" % akkaVersion)

  def akkaJs(name: String): Def.Initialize[sbt.ModuleID] =
    Def.setting("org.akka-js" %%% s"akkajs$name" % "2.2.6.14")

  val allAkka = {
    // Over-scoping this method to use non-scalajs dependencies
    def akka(name: String): Def.Initialize[sbt.ModuleID] =
      Def.setting("com.typesafe.akka" %% s"akka-$name" % akkaVersion)
    Def.setting(
      Seq(
        akka("actor").value,
        akka("actor-typed").value,
        akka("stream").value,
        akka("stream-typed").value,
        akka("slf4j").value,
        akka("testkit").value             % Test,
        akka("actor-testkit-typed").value % Test,
        akka("stream-testkit").value      % Test,
        "com.typesafe.akka"              %% "akka-http"         % akkaHttpVersion,
        "com.typesafe.akka"              %% "akka-http-core"    % akkaHttpVersion,
        "com.typesafe.akka"              %% "akka-http-testkit" % akkaHttpVersion % Test
      )
    )
  }

  val network = Def.setting(
    Seq(
      "org.bitlet"  % "weupnp"      % "0.1.4",
      "commons-net" % "commons-net" % "3.8.0"
    )
  )

  val circe = Def.setting(
    Seq(
      "io.circe" %%% "circe-core"    % circeVersion,
      "io.circe" %%% "circe-parser"  % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion
    )
  )

  val newType = Def.setting(
    Seq(
      "io.estatico" %%% "newtype" % "0.4.4"
    )
  )

  val guava = Def.setting(
    Seq(
      "com.google.guava" % "guava" % "31.0.1-jre"
    )
  )

  val misc = Def.setting(
    Seq(
      "com.chuusai" %%% "shapeless" % "2.3.7",
      "com.iheart"                 %% "ficus" % "1.5.1"
    ) ++ guava.value ++ newType.value
  )

  val monitoring = Def.setting(
    Seq(
      "io.kamon" %%% "kamon-core"     % kamonVersion,
      "io.kamon" %%% "kamon-bundle"   % kamonVersion % Runtime,
      "io.kamon" %%% "kamon-influxdb" % kamonVersion % Runtime,
      "io.kamon" %%% "kamon-zipkin"   % kamonVersion % Runtime
    )
  )

  val graal = Def.setting(
    Seq(
      "org.graalvm.sdk"     % "graal-sdk"   % graalVersion,
      "org.graalvm.js"      % "js"          % graalVersion,
      "org.graalvm.truffle" % "truffle-api" % graalVersion
    )
  )

  val cats = Def.setting(
    Seq(
      "org.typelevel" %%% "cats-core" % "2.3.1"
    )
  )

  val catsEffect = Def.setting(
    Seq(
      "org.typelevel" %%% "cats-effect" % "3.2.8"
    )
  )

  val scalacache = Def.setting(
    Seq(
      "com.github.cb372" %% "scalacache-caffeine" % "1.0.0-M4"
    )
  )

  val simulacrum = Def.setting(
    Seq(
      "org.typelevel" %%% "simulacrum" % "1.0.1"
    )
  )

  val bouncyCastle = Def.setting(
    Seq(
      "org.bouncycastle" % "bcprov-jdk15on" % "1.69"
    )
  )

  val levelDb = Def.setting(
    Seq(
      "org.ethereum"     % "leveldbjni-all" % "1.18.3",
      "org.iq80.leveldb" % "leveldb"        % "0.12"
    )
  )

  val scodec = Def.setting(
    Seq(
      "org.scodec" %%% "scodec-core" % "1.11.9"
    )
  )

  val scodecBits = Def.setting(
    Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.27"
    )
  )

  val node: Def.Initialize[Seq[ModuleID]] =
    Def.setting(
      Seq(
        akka("cluster").value,
        akka("remote").value,
        "com.typesafe"                                   % "config"         % "1.4.1",
        "com.lihaoyi" %%% "mainargs"                     % "0.2.1",
        "net.jpountz.lz4"                                % "lz4"            % "1.3.0",
        "com.github.julien-truffaut" %%% "monocle-core"  % "3.0.0-M6",
        "com.github.julien-truffaut" %%% "monocle-macro" % "3.0.0-M6",
        "org.ethereum"                                   % "leveldbjni-all" % "1.18.3",
        "org.iq80.leveldb"                               % "leveldb"        % "0.12",
        "org.mongodb.scala" %%% "mongo-scala-driver"     % "4.3.4"
      ) ++
      levelDb.value ++
      logging.value ++
      test.value ++
      it.value ++
      allAkka.value ++
      network.value ++
      circe.value ++
      misc.value ++
      monitoring.value
    )

  lazy val common: Def.Initialize[Seq[sbt.ModuleID]] =
    Def.setting(
      Seq(
        "org.typelevel" %%% "simulacrum"                       % simulacrumVersion,
        "org.scala-lang.modules" %%% "scala-collection-compat" % "2.6.0",
        "org.mongodb.scala" %%% "mongo-scala-driver"           % "4.3.2",
        "io.circe" %%% "circe-generic"                         % circeVersion,
        "org.scodec" %%% "scodec-core"                         % "1.11.8",
        "org.scodec" %%% "scodec-bits"                         % "1.1.30"
      ) ++
      logging.value ++
      scodecBits.value ++
      circe.value ++
      simulacrum.value ++
      test.value ++
      tools.value ++
      Seq(akka("actor-typed").value)
    )

  lazy val brambl: Def.Initialize[Seq[ModuleID]] =
    Def.setting(test.value ++ scodec.value ++ simulacrum.value)

  lazy val akkaHttpRpc: Def.Initialize[Seq[sbt.ModuleID]] =
    Def.setting(
      Seq(
        "de.heikoseeberger" %%% "akka-http-circe"              % "1.38.2",
        "io.circe" %%% "circe-optics"                          % circeVersion,
        "io.circe" %%% "circe-generic"                         % circeVersion,
        "org.scala-lang.modules" %%% "scala-collection-compat" % "2.6.0"
      ) ++
      circe.value ++
      allAkka.value ++
      test.value
    )

  lazy val toplRpc: Def.Initialize[Seq[sbt.ModuleID]] =
    Def.setting(
      Seq(
        "io.circe" %%% "circe-generic"                         % circeVersion,
        "org.scala-lang.modules" %%% "scala-collection-compat" % "2.6.0"
      ) ++
      circe.value ++
      test.value
    )

  lazy val crypto: Def.Initialize[Seq[ModuleID]] =
    Def.setting(
      Seq(
        "org.typelevel" %%% "simulacrum" % simulacrumVersion,
        "org.typelevel" %%% "cats-core"  % "2.7.0",
        "org.bouncycastle"               % "bcprov-jdk15on"  % "1.70",
        "org.whispersystems"             % "curve25519-java" % "0.5.0"
      ) ++
      scodecBits.value ++
      misc.value ++
      circe.value ++
      bouncyCastle.value ++
      cats.value ++
      simulacrum.value ++
      test.value
    )

  lazy val models: Def.Initialize[Seq[sbt.ModuleID]] =
    Def.setting(cats.value ++ simulacrum.value ++ newType.value ++ scodecBits.value)

  lazy val consensus: Def.Initialize[Seq[ModuleID]] =
    Def.setting(
      bouncyCastle.value ++ catsEffect.value ++ logging.value ++ scalacache.value
    )

  lazy val demo: Def.Initialize[Seq[ModuleID]] =
    Def.setting(
      Seq(
        akka("actor").value,
        akka("actor-typed").value,
        akka("stream").value,
        akka("actor-testkit-typed").value % Test
      ) ++ logging.value
    )

  lazy val tools: Def.Initialize[Seq[ModuleID]] =
    Def.setting(
      Seq(
        "org.mongodb.scala" %% "mongo-scala-driver" % "4.3.4"
      )
    )

  lazy val loadTesting: Def.Initialize[Seq[ModuleID]] =
    Def.setting(
      Seq(
        "com.lihaoyi"    %% "mainargs" % "0.2.1",
        "com.nike.fleam" %% "fleam"    % "7.0.0"
      ) ++
      allAkka.value ++
      circe.value
    )

}
