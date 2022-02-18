import sbt.Keys.{homepage, organization, test}
import sbtassembly.MergeStrategy

val scala212 = "2.12.15"
val scala213 = "2.13.6"

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
    parallelExecution := false
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
  crossScalaVersions := Seq(scala212, scala213),
  Test / testOptions ++= Seq(
    Tests.Argument("-oD", "-u", "target/test-reports"),
    Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
    Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDG")
  ),
  Test / parallelExecution := false,
  Test / logBuffered := false,
  classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat,
  Test / fork := false,
  Compile / run / fork := true,
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")
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
  Docker / packageName := "bifrost-node",
  dockerBaseImage := "ghcr.io/graalvm/graalvm-ce:java11-21.3.0",
  dockerUpdateLatest := true,
  dockerExposedPorts := Seq(9084, 9085),
  dockerExposedVolumes += "/opt/docker/.bifrost",
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
    node.jvm,
    common.jvm,
    akkaHttpRpc.jvm,
    models.jvm,
    models.js,
    typeclasses.jvm,
    typeclasses.js,
    toplRpc.jvm,
    benchmarking.jvm,
    crypto.jvm,
    crypto.js,
    brambl.jvm,
    models.jvm,
    models.js,
    algebras.jvm,
    algebras.js,
    minting.jvm,
    minting.js,
    byteCodecs.jvm,
    byteCodecs.js,
    tetraByteCodecs.jvm,
    tetraByteCodecs.js,
    consensus.jvm,
    consensus.js,
    demo.jvm,
    tools.jvm,
    scripting.jvm,
    scalajsDemo.js
  )

lazy val node = crossProject(JVMPlatform)
  .in(file("node"))
  .settings(
    name := "bifrost-node",
    commonSettings,
    assemblySettings("co.topl.BifrostApp"),
    dockerSettings,
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213), // The `monocle` library does not support Scala 2.12
    Compile / mainClass := Some("co.topl.BifrostApp"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.bifrost",
    libraryDependencies ++= Dependencies.node.value
  )
  .configs(IntegrationTest)
  .settings(
    IntegrationTest / parallelExecution := false
  )
  .dependsOn(common % "compile->compile;test->test", toplRpc, tools)
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)

lazy val common = crossProject(JVMPlatform)
  .in(file("common"))
  .settings(
    name := "common",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Dependencies.common.value
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

lazy val brambl = crossProject(JVMPlatform)
  .in(file("brambl"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "brambl",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Dependencies.brambl.value,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.brambl"
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(toplRpc, common, typeclasses, models % "compile->compile;test->test", scripting, tetraByteCodecs)

lazy val akkaHttpRpc = crossProject(JVMPlatform)
  .in(file("akka-http-rpc"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "akka-http-rpc",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Dependencies.akkaHttpRpc.value,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.akkahttprpc"
  )

lazy val models = crossProject(JVMPlatform, JSPlatform)
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
    libraryDependencies ++= Dependencies.models.value
  )
  .settings(libraryDependencies ++= Dependencies.test.value)

lazy val byteCodecs = crossProject(JVMPlatform, JSPlatform)
  .in(file("byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "byte-codecs",
    commonSettings,
    crossScalaVersions := Seq(scala213),
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.bytes"
  )
  .settings(
    libraryDependencies ++=
      Dependencies.test.value ++
      Dependencies.simulacrum.value ++
      Dependencies.scodec.value ++
      Dependencies.scodecBits.value ++
      Dependencies.cats.value
  )
  .jsSettings(
    libraryDependencies ++=
      Seq(Dependencies.akkaJs("actor").value)
  )
  .jvmSettings(
    libraryDependencies ++=
      Seq(Dependencies.akka("actor").value)
  )
  .settings(scalamacrosParadiseSettings)

lazy val tetraByteCodecs = crossProject(JVMPlatform, JSPlatform)
  .in(file("tetra-byte-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "tetra-byte-codecs",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.bytes.tetra"
  )
  .settings(libraryDependencies ++= Dependencies.test.value ++ Dependencies.guava.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, byteCodecs, crypto)

lazy val jsonCodecs = crossProject(JVMPlatform, JSPlatform)
  .in(file("json-codecs"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "json-codecs",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.codecs.json"
  )
  .settings(libraryDependencies ++= Dependencies.test.value ++ Dependencies.circe.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models)

lazy val typeclasses = crossProject(JVMPlatform, JSPlatform)
  .in(file("typeclasses"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "typeclasses",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.typeclasses"
  )
  .settings(libraryDependencies ++= Dependencies.test.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", crypto, tetraByteCodecs, jsonCodecs)

lazy val algebras = crossProject(JVMPlatform, JSPlatform)
  .in(file("algebras"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "algebras",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.algebras"
  )
  .settings(libraryDependencies ++= Dependencies.test.value ++ Seq(Dependencies.catsSlf4j.value % "test"))
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, crypto, tetraByteCodecs)

lazy val commonInterpreters = crossProject(JVMPlatform, JSPlatform)
  .in(file("common-interpreters"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "CommonInterpreters",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.interpreters"
  )
  .settings(
    libraryDependencies ++= Dependencies.test.value ++ Seq(
      Dependencies.catsSlf4j.value % "test"
    ) ++ Dependencies.cats.value ++ Dependencies.catsEffect.value ++ Dependencies.scalacache.value
  )
  .jsSettings(
    libraryDependencies ++=
      Seq(Dependencies.akkaJs("actortyped").value)
  )
  .jvmSettings(
    libraryDependencies ++=
      Seq(Dependencies.akka("actor-typed").value)
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models, crypto, tetraByteCodecs, algebras, typeclasses)

lazy val consensus = crossProject(JVMPlatform, JSPlatform)
  .in(file("consensus"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "consensus",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.consensus"
  )
  .settings(libraryDependencies ++= Dependencies.test.value)
  .settings(
    libraryDependencies ++= Dependencies.consensus.value
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    crypto,
    tetraByteCodecs,
    algebras % "compile->compile;test->test"
  )
  .enablePlugins(ScalaJSPlugin)

lazy val minting = crossProject(JVMPlatform, JSPlatform)
  .in(file("minting"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "minting",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.minting"
  )
  .settings(libraryDependencies ++= Dependencies.test.value ++ Dependencies.catsEffect.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    crypto,
    tetraByteCodecs,
    algebras % "compile->compile;test->test",
    consensus
  )
  .enablePlugins(ScalaJSPlugin)

lazy val demo = crossProject(JVMPlatform)
  .in(file("demo"))
  .settings(
    name := "demo",
    commonSettings,
    assemblySettings("co.topl.demo.TetraDemo"),
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213), // don't care about cross-compiling applications
    Compile / run / mainClass := Some("co.topl.demo.TetraDemo"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.demo",
    Docker / packageName := "bifrost-node",
    dockerExposedPorts := Seq(9084, 9085),
    dockerExposedVolumes += "/opt/docker/.bifrost",
    dockerLabels ++= Map(
      "bifrost.version" -> version.value
    )
  )
  .settings(libraryDependencies ++= Dependencies.test.value ++ Dependencies.demo.value ++ Dependencies.catsEffect.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", typeclasses, consensus, minting, scripting, commonInterpreters)
  .enablePlugins(BuildInfoPlugin, JavaAppPackaging, DockerPlugin)

lazy val scalajsSupport = crossProject(JSPlatform, JVMPlatform)
  .in(file("scalajs-support"))
  .settings(
    name := "scalajsSupport",
    publish / skip := true,
    libraryDependencies ++= Dependencies.cats.value,
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
  )

lazy val eligibilitySimulator = crossProject(JVMPlatform, JSPlatform)
  .in(file("eligibility-simulator"))
  .settings(
    name := "eligibilitySimulator",
    commonSettings,
    assemblySettings("co.topl.simulator.eligibility.EligibilitySimulator"),
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213), // don't care about cross-compiling applications
    Compile / run / mainClass := Some("co.topl.simulator.eligibility.EligibilitySimulator"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.simulator.eligibility",
    scalaJSUseMainModuleInitializer := true
  )
  .settings(libraryDependencies ++= Dependencies.test.value ++ Dependencies.demo.value ++ Dependencies.catsEffect.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models % "compile->compile;test->test",
    typeclasses,
    consensus,
    minting,
    commonInterpreters,
    scalajsSupport
  )
  .enablePlugins(BuildInfoPlugin)

lazy val scalajsDemo = crossProject(JSPlatform)
  .in(file("scalajs-demo"))
  .settings(
    name := "scalajsDemo",
    commonSettings,
    assemblySettings("co.topl.demo.ScalaJSDemo"),
    Defaults.itSettings,
    crossScalaVersions := Seq(scala213), // don't care about cross-compiling applications
    Compile / run / mainClass := Some("co.topl.demo.ScalaJSDemo"),
    publish / skip := true,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.demo",
    scalaJSUseMainModuleInitializer := true
  )
  .settings(libraryDependencies ++= Dependencies.catsEffect.value ++ Dependencies.scalajsDom.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(
    models,
    crypto,
    scalajsSupport,
    byteCodecs,
    tetraByteCodecs
  )
  .jsSettings(
    webpackBundlingMode := BundlingMode.LibraryAndApplication(),
    Compile / npmDependencies ++= Seq(
      "force-graph" -> "1.42.7",
      "blake2b" -> "2.1.4"
    ),
    stIgnore += "blake2b",
    /* we use a bit of functionality which can't be found in scala-js-dom */
    stUseScalaJsDom := false
  )
  .jsConfigure(project => project.enablePlugins(ScalaJSBundlerPlugin))
  .enablePlugins(BuildInfoPlugin, ScalablyTypedConverterPlugin)

lazy val scripting = crossProject(JVMPlatform)
  .in(file("scripting"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "scripting",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.scripting"
  )
  .settings(libraryDependencies ++= Dependencies.graal.value ++ Dependencies.catsEffect.value)
  .settings(libraryDependencies ++= Dependencies.test.value)
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test", typeclasses)

lazy val toplRpc = crossProject(JVMPlatform)
  .in(file("topl-rpc"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "topl-rpc",
    commonSettings,
    publishSettings,
    scalamacrosParadiseSettings,
    libraryDependencies ++= Dependencies.toplRpc.value,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.toplrpc"
  )
  .dependsOn(akkaHttpRpc, common)

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

lazy val benchmarking = crossProject(JVMPlatform)
  .in(file("benchmark"))
  .settings(
    name := "benchmark",
    commonSettings,
    publish / skip := true
  )
  .enablePlugins(JmhPlugin)
  .disablePlugins(sbtassembly.AssemblyPlugin)

lazy val crypto = crossProject(JVMPlatform, JSPlatform)
  .in(file("crypto"))
  .enablePlugins(BuildInfoPlugin, ScalaJSBundlerPlugin)
  .settings(
    name := "crypto",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.crypto",
    libraryDependencies ++= Dependencies.crypto.value
  )
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "blake2b" -> "2.1.4"
    ),
    stIgnore += "blake2b",
    webpackBundlingMode := BundlingMode.LibraryOnly()
  )
  .settings(scalamacrosParadiseSettings)
  .dependsOn(models % "compile->compile;test->test")
  .jsConfigure(project => project.enablePlugins(ScalaJSBundlerPlugin))
  .enablePlugins(ScalablyTypedConverterPlugin)

lazy val tools = crossProject(JVMPlatform)
  .in(file("tools"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "tools",
    commonSettings,
    publishSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.tools",
    libraryDependencies ++= Dependencies.tools.value
  )
  .dependsOn(common)

lazy val loadTesting = crossProject(JVMPlatform)
  .in(file("load-testing"))
  .settings(
    name := "load-testing",
    commonSettings,
    scalamacrosParadiseSettings,
    libraryDependencies ++= Dependencies.loadTesting.value
  )
  .dependsOn(common, brambl)

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; + test")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; + test")
