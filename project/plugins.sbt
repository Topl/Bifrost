// Comment to get more information during initialization
logLevel := Level.Error

addDependencyTreePlugin

Seq(
  "com.eed3si9n"            % "sbt-assembly"              % "2.0.0",
  "org.scalastyle"         %% "scalastyle-sbt-plugin"     % "1.0.0",
  "org.scoverage"           % "sbt-scoverage"             % "2.0.6",
  "com.github.sbt"          % "sbt-release"               % "1.1.0",
  "io.kamon"                % "sbt-kanela-runner"         % "2.0.14",
  "com.github.cb372"        % "sbt-explicit-dependencies" % "0.2.16",
  "pl.project13.scala"      % "sbt-jmh"                   % "0.4.3",
  "org.scalameta"           % "sbt-scalafmt"              % "2.5.0",
  "ch.epfl.scala"           % "sbt-scalafix"              % "0.10.4",
  "org.wartremover"         % "sbt-wartremover"           % "3.0.7",
  "com.github.sbt"          % "sbt-native-packager"       % "1.9.11",
  "com.eed3si9n"            % "sbt-buildinfo"             % "0.11.0",
  "com.github.sbt"          % "sbt-ci-release"            % "1.5.11",
  "net.bzzt"                % "sbt-reproducible-builds"   % "0.30",
  "com.lightbend.akka.grpc" % "sbt-akka-grpc"             % "2.1.6",
  "org.typelevel"           % "sbt-fs2-grpc"              % "2.5.9"
).map(addSbtPlugin)

// See: https://github.com/sbt/sbt/issues/6997
ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
