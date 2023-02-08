import sbt.Keys.{organization, test}
import sbtassembly.MergeStrategy

val scala213 = "2.13.10"

inThisBuild(
  List(
    organization := "co.topl",
    scalaVersion := scala213,
    versionScheme := Some("early-semver"),
    dynverSeparator := "-",
    version := dynverGitDescribeOutput.value.mkVersion(versionFmt, fallbackVersion(dynverCurrentDate.value)),
    dynver := {
      val d = new java.util.Date
      sbtdynver.DynVer.getGitDescribeOutput(d).mkVersion(versionFmt, fallbackVersion(d))
    },
    testFrameworks += TestFrameworks.MUnit
  )
)

Global / concurrentRestrictions += Tags.limit(Tags.Test, 1)

enablePlugins(ReproducibleBuildsPlugin, ReproducibleBuildsAssemblyPlugin)

lazy val commonSettings = Seq(
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  scalacOptions ++= commonScalacOptions,
  semanticdbEnabled := true, // enable SemanticDB for Scalafix
  semanticdbVersion := scalafixSemanticdb.revision, // use Scalafix compatible version
//  wartremoverErrors := Warts.unsafe, // settings for wartremover
  Compile / unmanagedSourceDirectories += {
    val sourceDir = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
      case _                       => sourceDir / "scala-2.12-"
    }
  },
  crossScalaVersions := Seq(scala213),
  Test / testOptions ++= Seq(
    Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
    Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDGG", "-u", "target/test-reports")
  ),
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1"),
  testFrameworks += TestFrameworks.MUnit
)

lazy val dockerSettings = Seq(
  dockerBaseImage := "eclipse-temurin:11-jre",
  dockerUpdateLatest := true,
  dockerLabels ++= Map(
    "bifrost.version" -> version.value
  ),
  dockerAliases := dockerAliases.value.flatMap { alias =>
    Seq(
      alias.withRegistryHost(Some("docker.io/toplprotocol")),
      alias.withRegistryHost(Some("ghcr.io/topl"))
    )
  }
)

lazy val nodeDockerSettings =
  dockerSettings ++ Seq(
    dockerExposedPorts := Seq(9084, 9085),
    dockerExposedVolumes += "/opt/docker/.bifrost",
    Docker / packageName := "bifrost-node"
  )

lazy val networkDelayerDockerSettings =
  dockerSettings ++ Seq(
    Docker / packageName := "network-delayer"
  )

lazy val testnetSimulationOrchestratorDockerSettings =
  dockerSettings ++ Seq(
    Docker / packageName := "testnet-simulation-orchestrator"
  )

def assemblySettings(main: String) = Seq(
  assembly / mainClass := Some(main),
  assembly / test := {},
  assemblyJarName := s"bifrost-node-${version.value}.jar",
  assembly / assemblyMergeStrategy ~= { old: (String => MergeStrategy) =>
    {
      case ps if ps.endsWith(".SF")  => MergeStrategy.discard
      case ps if ps.endsWith(".DSA") => MergeStrategy.discard
      case ps if ps.endsWith(".RSA") => MergeStrategy.discard
      case ps if ps.endsWith(".xml") => MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "module-info.class" =>
        MergeStrategy.discard // https://github.com/sbt/sbt-assembly/issues/370
      case x if x.contains("simulacrum")               => MergeStrategy.last
      case PathList("org", "iq80", "leveldb", xs @ _*) => MergeStrategy.first
      case PathList("module-info.java")                => MergeStrategy.discard
      case PathList("local.conf")                      => MergeStrategy.discard
      case "META-INF/truffle/instrument"               => MergeStrategy.concat
      case "META-INF/truffle/language"                 => MergeStrategy.rename
      case x if x.contains("google/protobuf")          => MergeStrategy.last
      case x                                           => old(x)
    }
  }
)

lazy val scalamacrosParadiseSettings =
  Seq(
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    )
  )

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked",
  "-Ywarn-unused:-implicits,-privates,_",
  "-Yrangepos"
)

javaOptions ++= Seq(
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

connectInput / run := true
outputStrategy := Some(StdoutOutput)

connectInput / run := true
outputStrategy := Some(StdoutOutput)

def versionFmt(out: sbtdynver.GitDescribeOutput): String = {
  val dirtySuffix = out.dirtySuffix.dropPlus.mkString("-", "")
  if (out.isCleanAfterTag) out.ref.dropPrefix + dirtySuffix // no commit info if clean after tag
  else out.ref.dropPrefix + out.commitSuffix.mkString("-", "-", "") + dirtySuffix
}

def fallbackVersion(d: java.util.Date): String = s"HEAD-${sbtdynver.DynVer timestamp d}"

lazy val bifrost = project
  .in(file("."))
  .settings(
    moduleName := "bifrost",
    commonSettings,
    publish / skip := true,
    crossScalaVersions := Nil
  )
  .configs(IntegrationTest)
  .aggregate(
    node,
    typeclasses,
    toplGrpc,
    crypto,
    catsAkka,
    models, // TODO remove BN-714 PR v2
    numerics,
    eventTree,
    algebras,
    actor,
    commonInterpreters,
    minting,
    networking,
    byteCodecs,
    tetraByteCodecs,
    consensus,
    ledger,
    blockchain,
    levelDbStore,
    commonApplication,
    networkDelayer,
    genusLibrary,
    genusServer,
    transactionGenerator,
    testnetSimulationOrchestrator
  )

lazy val node = project
  .in(file("node"))
  .settings(
    name := "bifrost-node",
    commonSettings,
    assemblySettings("co.topl.node.NodeApp"),
    assemblyJarName := s"bifrost-node-${version.value}.jar",
    nodeDockerSettings,
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213),
    Compile / mainClass := Some("co.topl.node.NodeApp"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.node",
    libraryDependencies ++= Dependencies.node
  )
  .configs(IntegrationTest)
  .settings(
    IntegrationTest / parallelExecution := false,
    classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat // required for correct loading https://github.com/kamon-io/sbt-kanela-runner
  )
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    consensus,
    minting,
    commonInterpreters,
    networking,
    catsAkka,
    toplGrpc,
    blockchain,
    levelDbStore,
    commonApplication
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)
  .settings(scalamacrosParadiseSettings)

lazy val networkDelayer = project
  .in(file("network-delayer"))
  .settings(
    name := "network-delayer",
    commonSettings,
    coverageEnabled := false,
    assemblySettings("co.topl.networkdelayer.NetworkDelayer"),
    assemblyJarName := s"network-delayer-${version.value}.jar",
    networkDelayerDockerSettings,
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213),
    Compile / mainClass := Some("co.topl.networkdelayer.NetworkDelayer"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.networkdelayer",
    libraryDependencies ++= Dependencies.networkDelayer
  )
  .configs(IntegrationTest)
  .settings(
    IntegrationTest / parallelExecution := false
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(catsAkka, commonApplication)

lazy val testnetSimulationOrchestrator = project
  .in(file("testnet-simulation-orchestrator"))
  .settings(
    name := "testnet-simulation-orchestrator",
    commonSettings,
    coverageEnabled := false,
    assemblySettings("co.topl.testnetsimulationorchestrator.app.Orchestrator"),
    assemblyJarName := s"testnet-simulation-orchestrator-${version.value}.jar",
    testnetSimulationOrchestratorDockerSettings,
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213),
    Compile / mainClass := Some("co.topl.testnetsimulationorchestrator.app.Orchestrator"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.testnetsimulationorchestator",
    libraryDependencies ++= Dependencies.testnetSimulationOrchestator
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(commonApplication, transactionGenerator)

lazy val commonApplication = project
  .in(file("common-application"))
  .settings(
    name := "common-application",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    libraryDependencies ++= Dependencies.commonApplication
  )
  .dependsOn(catsAkka)
  .settings(scalamacrosParadiseSettings)

// TODO remve BN-714 , PR v2
lazy val models = project
  .in(file("models"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "models",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.models"
  )
  .settings(scalamacrosParadiseSettings)
  .settings(
    libraryDependencies ++= Dependencies.models
  )
  .settings(libraryDependencies ++= Dependencies.test)

lazy val numerics = project
  .in(file("numerics"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "numerics",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.numerics"
  )
  .settings(scalamacrosParadiseSettings)
  .settings(libraryDependencies ++= Dependencies.test ++ Dependencies.scalacache)
  .dependsOn(models)

lazy val eventTree = project
  .in(file("event-tree"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "event-tree",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.eventtree"
  )
  .settings(libraryDependencies ++= Dependencies.eventTree)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(algebras % "compile->test")

lazy val byteCodecs = project
  .in(file("byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "byte-codecs",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.bytes"
  )
  .settings(
    libraryDependencies ++= Dependencies.byteCodecs
  )
  .settings(scalamacrosParadiseSettings)

lazy val tetraByteCodecs = project
  .in(file("tetra-byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "tetra-byte-codecs",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.bytes.tetra"
  )
  .settings(libraryDependencies ++= Dependencies.test ++ Dependencies.guava ++ Seq(Dependencies.protobufSpecs))
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", byteCodecs % "compile->compile;test->test", crypto)

lazy val typeclasses: Project = project
  .in(file("typeclasses"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "typeclasses",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.typeclasses"
  )
  .settings(
    libraryDependencies ++= Dependencies.test ++ Dependencies.logging
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", crypto, tetraByteCodecs)

lazy val algebras = project
  .in(file("algebras"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "algebras",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.algebras"
  )
  .settings(libraryDependencies ++= Dependencies.algebras)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, crypto, tetraByteCodecs)

lazy val actor = project
  .in(file("actor"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "actor",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.actor"
  )
  .settings(libraryDependencies ++= Dependencies.actor)
  .dependsOn(
    munitScalamock % "test->test"
  )

lazy val commonInterpreters = project
  .in(file("common-interpreters"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "common-interpreters",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.commoninterpreters"
  )
  .settings(libraryDependencies ++= Dependencies.commonInterpreters)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models,
    algebras,
    typeclasses,
    byteCodecs,
    tetraByteCodecs,
    catsAkka,
    eventTree,
    munitScalamock % "test->test"
  )

lazy val consensus = project
  .in(file("consensus"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "consensus",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.consensus"
  )
  .settings(libraryDependencies ++= Dependencies.test)
  .settings(
    libraryDependencies ++= Dependencies.consensus
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    crypto,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    numerics,
    eventTree,
    munitScalamock % "test->test"
  )

lazy val minting = project
  .in(file("minting"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "minting",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.minting"
  )
  .settings(libraryDependencies ++= Dependencies.minting)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    crypto,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    consensus,
    catsAkka,
    ledger,
    munitScalamock     % "test->test",
    commonInterpreters % "test->test"
  )

lazy val networking = project
  .in(file("networking"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "networking",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.networking"
  )
  .settings(libraryDependencies ++= Dependencies.networking)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    crypto,
    byteCodecs,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    consensus,
    commonInterpreters,
    catsAkka,
    eventTree,
    ledger,
    actor
  )

lazy val transactionGenerator = project
  .in(file("transaction-generator"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "transaction-generator",
    commonSettings,
    coverageEnabled := false,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.transactiongenerator"
  )
  .settings(libraryDependencies ++= Dependencies.transactionGenerator)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    crypto,
    byteCodecs,
    tetraByteCodecs,
    munitScalamock,
    algebras,
    toplGrpc,
    commonApplication,
    commonInterpreters
  )

lazy val ledger = project
  .in(file("ledger"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "ledger",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.ledger"
  )
  .settings(libraryDependencies ++= Dependencies.ledger)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models   % "compile->compile;test->test",
    algebras % "compile->compile;test->test",
    typeclasses,
    eventTree,
    munitScalamock % "test->test"
  )

lazy val blockchain = project
  .in(file("blockchain"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "blockchain",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.blockchain"
  )
  .settings(libraryDependencies ++= Dependencies.blockchain)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models   % "compile->compile;test->test",
    algebras % "compile->compile;test->test",
    typeclasses,
    eventTree,
    ledger,
    munitScalamock % "test->test",
    consensus,
    minting,
    commonInterpreters,
    networking % "compile->compile;test->test",
    catsAkka,
    toplGrpc
  )

lazy val toplGrpc = project
  .in(file("topl-grpc"))
  .settings(
    name := "topl-grpc",
    commonSettings,
    libraryDependencies ++= Dependencies.toplGrpc
  )
  .dependsOn(
    models % "compile->compile;test->test",
    byteCodecs,
    tetraByteCodecs,
    algebras,
    catsAkka,
    typeclasses,
    munitScalamock % "test->test"
  )

lazy val levelDbStore = project
  .in(file("level-db-store"))
  .settings(
    name := "level-db-store",
    commonSettings,
    libraryDependencies ++= Dependencies.levelDbStore
  )
  .dependsOn(
    byteCodecs,
    algebras,
    catsAkka
  )

lazy val crypto = project
  .in(file("crypto"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "crypto",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.crypto",
    libraryDependencies ++= Dependencies.crypto
  )
  .dependsOn(models % "compile->compile;test->test")

lazy val catsAkka = project
  .in(file("cats-akka"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "cats-akka",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.catsakka",
    libraryDependencies ++= Dependencies.catsAkka
  )
  .settings(scalamacrosParadiseSettings)

lazy val genusServer = project
  .in(file("genus-server"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "genus-server",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.genusServer",
    libraryDependencies ++= Dependencies.genusServer
  )
  .dependsOn(genusLibrary)

lazy val genusLibrary = project
  .in(file("genus-library"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "genus-library",
    commonSettings,
    scalamacrosParadiseSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.genusLibrary",
    libraryDependencies ++= Dependencies.genusLibrary
  )
  .dependsOn(
    typeclasses,
    models % "compile->compile;test->test",
    tetraByteCodecs,
    toplGrpc,
    munitScalamock % "test->test",
    numerics       % "test->compile"
  )

lazy val munitScalamock = project
  .in(file("munit-scalamock"))
  .settings(
    name := "munit-scalamock",
    commonSettings,
    libraryDependencies ++= Dependencies.munitScalamock
  )

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; +test; it:compile")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; +test; it:compile")
addCommandAlias("checkPRTestQuick", s"; scalafixAll --check; scalafmtCheckAll; testQuick; it:compile")
