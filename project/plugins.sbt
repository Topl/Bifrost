// Comment to get more information during initialization
logLevel := Level.Error

addDependencyTreePlugin
addSbtPlugin("com.sonar-scala" % "sbt-sonar" % "2.3.0")

Seq(
  "com.eed3si9n"       % "sbt-assembly"              % "1.1.1",
  "org.scalastyle"    %% "scalastyle-sbt-plugin"     % "1.0.0",
  "org.scoverage"      % "sbt-scoverage"             % "1.9.3",
  "com.github.sbt"     % "sbt-release"               % "1.1.0",
  "io.kamon"           % "sbt-kanela-runner"         % "2.0.12",
  "com.github.cb372"   % "sbt-explicit-dependencies" % "0.2.16",
  "pl.project13.scala" % "sbt-jmh"                   % "0.4.3",
  "org.scalameta"      % "sbt-scalafmt"              % "2.4.6",
  "ch.epfl.scala"      % "sbt-scalafix"              % "0.9.34",
  "org.wartremover"    % "sbt-wartremover"           % "2.4.18",
  "com.github.sbt"     % "sbt-native-packager"       % "1.9.9",
  "com.eed3si9n"       % "sbt-buildinfo"             % "0.11.0",
  "com.geirsson"       % "sbt-ci-release"            % "1.5.7",
  "net.bzzt"           % "sbt-reproducible-builds"   % "0.30",
  "com.lightbend.akka.grpc" % "sbt-akka-grpc" % "2.1.4"
).map(addSbtPlugin)
