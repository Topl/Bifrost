import com.typesafe.sbt.packager.docker.{DockerChmodType, ExecCmd}
import sbt.Keys.{organization, test}
import sbtassembly.MergeStrategy
import NativePackagerHelper.*

val scala213 = "2.13.13"

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
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.3" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1"),
  testFrameworks += TestFrameworks.MUnit,
  dependencyOverrides ++= Dependencies.protobufSpecs ++ Seq(Dependencies.quivr4s)
)

lazy val dockerSettings = Seq(
  dockerBaseImage := "eclipse-temurin:11-jre",
  dockerUpdateLatest := sys.env.get("DOCKER_PUBLISH_LATEST_TAG").fold(false)(_.toBoolean),
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
    Docker / packageName := "bifrost-node",
    dockerExposedVolumes += "/bifrost",
    dockerExposedVolumes += "/bifrost-staking",
    dockerEnvVars ++= Map(
      "BIFROST_APPLICATION_DATA_DIR"    -> "/bifrost/data/{genesisBlockId}",
      "BIFROST_APPLICATION_STAKING_DIR" -> "/bifrost-staking/{genesisBlockId}",
      "BIFROST_CONFIG_FILE"             -> "/bifrost/config/user.yaml"
    ),
    dockerAliases ++= (
      if (sys.env.get("DOCKER_PUBLISH_DEV_TAG").fold(false)(_.toBoolean))
        Seq(
          DockerAlias(Some("docker.io"), Some("toplprotocol"), "bifrost-node", Some("dev")),
          DockerAlias(Some("ghcr.io"), Some("topl"), "bifrost-node", Some("dev"))
        )
      else Seq()
      )
  )

lazy val genusDockerSettings =
  dockerSettings ++ Seq(
    dockerExposedPorts := Seq(9084),
    Docker / packageName := "genus",
    dockerExposedVolumes += "/genus",
    dockerAliases ++= (
      if (sys.env.get("DOCKER_PUBLISH_DEV_TAG").fold(false)(_.toBoolean))
        Seq(
          DockerAlias(Some("docker.io"), Some("toplprotocol"), "genus", Some("dev")),
          DockerAlias(Some("ghcr.io"), Some("topl"), "genus", Some("dev"))
        )
      else Seq()
      )
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
      case "META-INF/io.netty.versions.properties"     => MergeStrategy.last
      case "META-INF/truffle/instrument"               => MergeStrategy.concat
      case "META-INF/truffle/language"                 => MergeStrategy.rename
      case "META-INF/okio.kotlin_module"               => MergeStrategy.first
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
  "-Ywarn-unused:_",
  "-Yrangepos",
  "-Ywarn-macros:after"
)

javaOptions ++= Seq(
  // Force the JVM to exit the first time it encounters an OOM error.  By default, it might not exit.
  "-XX:+ExitOnOutOfMemoryError",
  // Disables the shared memory space for JVM stats, thus preventing external processes from viewing memory/CPU stats.
  // Disabled to prevent a potential security threat
  "-XX:+PerfDisableSharedMem"
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
  .aggregate(
    node,
    typeclasses,
    config,
    toplGrpc,
    nodeCrypto,
    catsUtils,
    models,
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
    blockchainCore,
    blockchain,
    levelDbStore,
    commonApplication,
    networkDelayer,
    genus,
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
    crossScalaVersions := Seq(scala213),
    Compile / mainClass := Some("co.topl.node.NodeApp"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.node",
    libraryDependencies ++= Dependencies.node
  )
  .settings(
    classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat // required for correct loading https://github.com/kamon-io/sbt-kanela-runner
  )
  .dependsOn(
    models % "compile->compile;test->test",
    config,
    typeclasses,
    consensus,
    minting,
    commonInterpreters,
    networking,
    catsUtils,
    toplGrpc,
    blockchainCore,
    blockchain,
    levelDbStore,
    commonApplication,
    genus
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)
  .settings(scalamacrosParadiseSettings)

lazy val config = project
  .in(file("config"))
  .settings(
    name := "config",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    libraryDependencies ++= Dependencies.monocle
  )
  .dependsOn(models, numerics)
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
    crossScalaVersions := Seq(scala213),
    Compile / mainClass := Some("co.topl.networkdelayer.NetworkDelayer"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.networkdelayer",
    libraryDependencies ++= Dependencies.networkDelayer
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(catsUtils, commonApplication)

lazy val testnetSimulationOrchestrator = project
  .in(file("testnet-simulation-orchestrator"))
  .settings(
    name := "testnet-simulation-orchestrator",
    commonSettings,
    coverageEnabled := false,
    assemblySettings("co.topl.testnetsimulationorchestrator.app.Orchestrator"),
    assemblyJarName := s"testnet-simulation-orchestrator-${version.value}.jar",
    testnetSimulationOrchestratorDockerSettings,
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
  .dependsOn(catsUtils)
  .settings(scalamacrosParadiseSettings)

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
    libraryDependencies ++= Dependencies.models ++ Dependencies.mUnitTest
  )
  .dependsOn(munitScalamock % "test->test")

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
  .settings(libraryDependencies ++= Dependencies.mUnitTest ++ Dependencies.scalacache)
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
    libraryDependencies ++= Dependencies.byteCodecs ++ Dependencies.protobufSpecs
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(munitScalamock % "test->test")

lazy val tetraByteCodecs = project
  .in(file("tetra-byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "tetra-byte-codecs",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.bytes.tetra"
  )
  .settings(libraryDependencies ++= Dependencies.munitScalamock ++ Dependencies.protobufSpecs)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models     % "compile->compile;test->test",
    byteCodecs % "compile->compile;test->test",
    nodeCrypto % "compile->compile;test->test"
  )

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
    libraryDependencies ++= Dependencies.mUnitTest ++ Dependencies.logging
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", nodeCrypto, tetraByteCodecs)

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
  .dependsOn(models, nodeCrypto, tetraByteCodecs, munitScalamock % "test->test")

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
    catsUtils,
    eventTree,
    munitScalamock % "test->test",
    levelDbStore   % "test->test"
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
  .settings(libraryDependencies ++= Dependencies.mUnitTest)
  .settings(
    libraryDependencies ++= Dependencies.consensus
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    nodeCrypto,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    numerics,
    eventTree,
    commonInterpreters % "compile->test",
    munitScalamock     % "test->test"
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
    nodeCrypto,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    consensus,
    catsUtils,
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
    config,
    typeclasses,
    nodeCrypto,
    byteCodecs,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    consensus,
    commonInterpreters,
    catsUtils,
    eventTree,
    ledger,
    actor,
    munitScalamock % "test->test",
    blockchainCore
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
    nodeCrypto,
    byteCodecs,
    tetraByteCodecs,
    munitScalamock,
    algebras,
    toplGrpc,
    commonApplication,
    commonInterpreters,
    numerics
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
    munitScalamock % "test->test",
    numerics
  )

lazy val blockchainCore = project
  .in(file("blockchain-core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "blockchain-core",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.blockchaincore"
  )
  .settings(libraryDependencies ++= Dependencies.blockchain)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models   % "compile->compile;test->test",
    algebras % "compile->compile;test->test",
    config,
    typeclasses,
    eventTree,
    ledger,
    munitScalamock % "test->test",
    consensus,
    minting,
    commonInterpreters,
    catsUtils
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
    config,
    typeclasses,
    eventTree,
    ledger,
    munitScalamock % "test->test",
    consensus,
    minting,
    commonInterpreters,
    networking % "compile->compile;test->test",
    catsUtils,
    toplGrpc,
    blockchainCore
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
    catsUtils,
    typeclasses,
    munitScalamock % "test->test",
    blockchainCore
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
    algebras % "compile->compile;test->test",
    catsUtils
  )

lazy val nodeCrypto = project
  .in(file("node-crypto"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "node-crypto",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.nodecrypto",
    libraryDependencies ++= Dependencies.crypto
  )

lazy val catsUtils = project
  .in(file("cats-utils"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "cats-utils",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.catsUtils",
    libraryDependencies ++= Dependencies.catsUtils
  )
  .settings(scalamacrosParadiseSettings)

lazy val genus = project
  .in(file("genus"))
  .settings(
    name := "genus",
    commonSettings,
    scalamacrosParadiseSettings,
    publish / skip := true,
    crossScalaVersions := Seq(scala213),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.genus",
    libraryDependencies ++= Dependencies.genus
  )
  .settings(genusDockerSettings)
  .dependsOn(
    typeclasses,
    models % "compile->compile;test->test",
    tetraByteCodecs,
    toplGrpc,
    commonInterpreters,
    commonApplication,
    munitScalamock % "test->test",
    numerics       % "test->compile"
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)

lazy val munitScalamock = project
  .in(file("munit-scalamock"))
  .settings(
    name := "munit-scalamock",
    commonSettings,
    libraryDependencies ++= Dependencies.munitScalamock
  )

lazy val nodeIt = project
  .in(file("node-it"))
  .settings(
    name := "node-it",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    libraryDependencies ++= Dependencies.nodeIt
  )
  .dependsOn(
    node,
    transactionGenerator % "test->compile"
  )

lazy val byzantineIt = project
  .in(file("byzantine-it"))
  .settings(
    name := "byzantine-it",
    commonSettings,
    Test / parallelExecution := false,
    libraryDependencies ++= Dependencies.byzantineIt
  )
  .dependsOn(
    node
  )

lazy val integration = (project in file("integration"))
  .aggregate(nodeIt, byzantineIt)

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; +test; integration/Test/compile")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; +test; integration/Test/compile")
addCommandAlias("checkPRTestQuick", s"; scalafixAll --check; scalafmtCheckAll; testQuick; integration/Test/compile")
