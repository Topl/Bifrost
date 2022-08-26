import sbt.Keys.{homepage, organization, test}
import sbtassembly.MergeStrategy

val scala212 = "2.12.16"
val scala213 = "2.13.8"

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
  // Enable PartialUnification in Scala 2.12.  Scala 2.13 fixes this by default
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) if v <= 12 =>
      Seq(
        "-Ypartial-unification"
      )
    case _ =>
      Nil
  }),
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
  crossScalaVersions := Seq(scala212, scala213),
  Test / testOptions ++= Seq(
    Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
    Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDGG", "-u", "target/test-reports")
  ),
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1"),
  testFrameworks += TestFrameworks.MUnit
)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/Topl/Bifrost")),
  licenses := Seq("MPL2.0" -> url("https://www.mozilla.org/en-US/MPL/2.0/")),
  Test / publishArtifact := false,
  pomIncludeRepository := { _ => false },
  usePgpKeyHex("CEE1DC9E7C8E9AF4441D5EB9E35E84257DCF8DCB"),
  pomExtra :=
    <developers>
      <developer>
        <id>scasplte2</id>
        <name>James Aman</name>
      </developer>
      <developer>
        <id>tuxman</id>
        <name>Nicholas Edmonds</name>
      </developer>
    </developers>
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

lazy val tetraNodeDockerSettings =
  dockerSettings ++ Seq(
    dockerExposedPorts := Seq(9084, 9085),
    dockerExposedVolumes += "/opt/docker/.bifrost",
    Docker / packageName := "bifrost-node-tetra"
  )

lazy val dionNodeDockerSettings =
  dockerSettings ++ Seq(
    dockerExposedPorts := Seq(9084, 9085),
    dockerExposedVolumes += "/opt/docker/.bifrost",
    Docker / packageName := "bifrost-node"
  )

def assemblySettings(main: String) = Seq(
  assembly / mainClass := Some(main),
  assembly / test := {},
  assemblyJarName := s"bifrost-node-${version.value}.jar",
  assembly / assemblyMergeStrategy ~= { old: ((String) => MergeStrategy) =>
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
  },
  assembly / assemblyExcludedJars := {
    val cp = (assembly / fullClasspath).value
    cp filter { el => el.data.getName == "ValkyrieInstrument-1.0.jar" }
  }
)

lazy val scalamacrosParadiseSettings =
  Seq(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v <= 12 =>
          Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
          )
        case _ =>
          Nil
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          Seq(
            "-Ymacro-annotations"
          )
        case _ =>
          Nil
      }
    }
  )

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked",
  "-Xlint:",
  "-Ywarn-unused:-implicits,-privates"
)

javaOptions ++= Seq(
  "-Xbootclasspath/a:ValkyrieInstrument-1.0.jar",
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
    nodeTetra,
    common,
    akkaHttpRpc,
    typeclasses,
    toplRpc,
    crypto,
    catsAkka,
    brambl,
    models,
    numerics,
    eventTree,
    algebras,
    commonInterpreters,
    minting,
    networking,
    byteCodecs,
    tetraByteCodecs,
    consensus,
    ledger,
    blockchain,
    demo,
    tools,
    scripting,
    genus,
    levelDbStore
  )

lazy val node = project
  .in(file("node"))
  .settings(
    name := "bifrost-node",
    commonSettings,
    assemblySettings("co.topl.BifrostApp"),
    dionNodeDockerSettings,
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213), // The `monocle` library does not support Scala 2.12
    Compile / mainClass := Some("co.topl.BifrostApp"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.bifrost",
    libraryDependencies ++= Dependencies.nodeDion
  )
  .configs(IntegrationTest)
  .settings(
    IntegrationTest / parallelExecution := false
  )
  .dependsOn(common % "compile->compile;test->test", toplRpc, tools, genus, jsonCodecs)
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)

lazy val nodeTetra = project
  .in(file("node-tetra"))
  .settings(
    name := "bifrost-node-tetra",
    commonSettings,
    assemblySettings("co.topl.node.NodeApp"),
    assemblyJarName := s"bifrost-node-tetra-${version.value}.jar",
    tetraNodeDockerSettings,
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213),
    Compile / mainClass := Some("co.topl.node.NodeApp"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.node",
    libraryDependencies ++= Dependencies.nodeTetra
  )
  .configs(IntegrationTest)
  .settings(
    IntegrationTest / parallelExecution := false
  )
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    consensus,
    minting,
    scripting,
    commonInterpreters,
    networking,
    catsAkka,
    toplGrpc,
    blockchain
  )
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)

lazy val common = project
  .in(file("common"))
  .settings(
    name := "common",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Dependencies.common
  )
  .dependsOn(crypto, typeclasses, models % "compile->compile;test->test")
  .settings(scalamacrosParadiseSettings)

//lazy val chainProgram = project
//  .in(file("chain-program"))
//  .settings(
//    name := "chain-program",
//    commonSettings,
//    publish / skip := true,
//    libraryDependencies ++= Dependencies.chainProgram
//  )
//  .dependsOn(common)
//  .disablePlugins(sbtassembly.AssemblyPlugin)

lazy val brambl = project
  .in(file("brambl"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "brambl",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Dependencies.brambl,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.brambl"
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(toplRpc, common, typeclasses, models % "compile->compile;test->test", scripting, tetraByteCodecs, toplGrpc)

lazy val akkaHttpRpc = project
  .in(file("akka-http-rpc"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "akka-http-rpc",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Dependencies.akkaHttpRpc,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.akkahttprpc"
  )

lazy val models = project
  .in(file("models"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "models",
    commonSettings,
    publishSettings,
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
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.numerics"
  )
  .settings(scalamacrosParadiseSettings)
  .settings(libraryDependencies ++= Dependencies.test ++ Dependencies.scalacache)
  .dependsOn(algebras, typeclasses, models)

lazy val eventTree = project
  .in(file("event-tree"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "event-tree",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.eventtree"
  )
  .settings(libraryDependencies ++= Dependencies.test ++ Dependencies.catsEffect)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, typeclasses, algebras % "compile->compile;test->test")

lazy val byteCodecs = project
  .in(file("byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "byte-codecs",
    commonSettings,
    publishSettings,
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
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.bytes.tetra"
  )
  .settings(libraryDependencies ++= Dependencies.test ++ Dependencies.guava)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", byteCodecs % "compile->compile;test->test", crypto)

lazy val jsonCodecs = project
  .in(file("json-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "json-codecs",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.json"
  )
  .settings(libraryDependencies ++= Dependencies.test ++ Dependencies.circe ++ Dependencies.scodec)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, crypto, tetraByteCodecs)

lazy val typeclasses: Project = project
  .in(file("typeclasses"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "typeclasses",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.typeclasses"
  )
  .settings(libraryDependencies ++= Dependencies.test)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", crypto, tetraByteCodecs, jsonCodecs)

lazy val algebras = project
  .in(file("algebras"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "algebras",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.algebras"
  )
  .settings(libraryDependencies ++= Dependencies.algebras)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, crypto, tetraByteCodecs)

lazy val commonInterpreters = project
  .in(file("common-interpreters"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "common-interpreters",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.commoninterpreters"
  )
  .settings(libraryDependencies ++= Dependencies.commonInterpreters)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, algebras, typeclasses, byteCodecs, tetraByteCodecs, catsAkka, eventTree)

lazy val consensus = project
  .in(file("consensus"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "consensus",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    publishSettings,
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
    publishSettings,
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
    ledger
  )

lazy val networking = project
  .in(file("networking"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "networking",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    publishSettings,
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
    ledger
  )

lazy val ledger = project
  .in(file("ledger"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "ledger",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    publishSettings,
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
    publishSettings,
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
    scripting,
    commonInterpreters,
    networking,
    catsAkka,
    toplGrpc
  )

lazy val demo = project
  .in(file("demo"))
  .settings(
    name := "demo",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    Compile / run / mainClass := Some("co.topl.demo.TetraDemo"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.demo",
  )
  .settings(libraryDependencies ++= Dependencies.test ++ Dependencies.demo ++ Dependencies.catsEffect)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    consensus,
    minting,
    scripting,
    commonInterpreters,
    networking,
    catsAkka,
    toplGrpc,
    blockchain
  )
  .enablePlugins(BuildInfoPlugin)

lazy val scripting: Project = project
  .in(file("scripting"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "scripting",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.scripting"
  )
  .settings(
    libraryDependencies ++= Dependencies.graal ++ Dependencies.catsEffect ++ Dependencies.circe ++ Dependencies.simulacrum
  )
  .settings(libraryDependencies ++= Dependencies.test)
  .settings(scalamacrosParadiseSettings)

lazy val toplRpc = project
  .in(file("topl-rpc"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "topl-rpc",
    commonSettings,
    publishSettings,
    scalamacrosParadiseSettings,
    libraryDependencies ++= Dependencies.toplRpc,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.toplrpc"
  )
  .dependsOn(akkaHttpRpc, common, jsonCodecs)

lazy val toplGrpc = project
  .in(file("topl-grpc"))
  .settings(
    name := "topl-grpc",
    commonSettings,
    libraryDependencies ++= Dependencies.toplGrpc
  )
  .enablePlugins(AkkaGrpcPlugin)
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

// This module has fallen out of sync with the rest of the codebase and is not currently needed
//lazy val gjallarhorn = project
//  .in(file("gjallarhorn"))
//  .settings(
//    name := "gjallarhorn",
//    commonSettings,
//    publish / skip := true,
//    Defaults.itSettings,
//    libraryDependencies ++= Dependencies.gjallarhorn
//  )
//  .dependsOn(crypto, common)
//  .configs(IntegrationTest)
//  .disablePlugins(sbtassembly.AssemblyPlugin)
//  .settings(scalamacrosParadiseSettings)

lazy val benchmarking = project
  .in(file("benchmark"))
  .settings(
    name := "benchmark",
    commonSettings,
    publish / skip := true,
    libraryDependencies ++= Dependencies.benchmarking
  )
  .enablePlugins(JmhPlugin)
  .disablePlugins(sbtassembly.AssemblyPlugin)

lazy val crypto = project
  .in(file("crypto"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "crypto",
    commonSettings,
    publishSettings,
    scalamacrosParadiseSettings,
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
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.catsakka",
    libraryDependencies ++= Dependencies.catsAkka
  )
  .settings(scalamacrosParadiseSettings)

lazy val tools = project
  .in(file("tools"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "tools",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.tools",
    libraryDependencies ++= Dependencies.mongoDb
  )
  .dependsOn(common)

lazy val loadTesting = project
  .in(file("load-testing"))
  .settings(
    name := "load-testing",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    scalamacrosParadiseSettings,
    libraryDependencies ++= Dependencies.loadTesting
  )
  .dependsOn(common, brambl)

lazy val genus = project
  .in(file("genus"))
  .settings(
    name := "genus",
    commonSettings,
    scalamacrosParadiseSettings,
    libraryDependencies ++= Dependencies.genus
  )
  .enablePlugins(AkkaGrpcPlugin)
  .dependsOn(common)

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
