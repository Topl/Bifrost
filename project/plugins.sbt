// Comment to get more information during initialization
logLevel := Level.Error

Seq(
  "com.eed3si9n"       % "sbt-assembly"              % "1.0.0",
  "org.scalastyle"    %% "scalastyle-sbt-plugin"     % "1.0.0",
  "org.scoverage"      % "sbt-scoverage"             % "1.8.2",
  "com.github.sbt"     % "sbt-release"               % "1.0.15",
  "net.virtual-void"   % "sbt-dependency-graph"      % "0.9.2",
  "io.kamon"           % "sbt-kanela-runner"         % "2.0.10",
  "com.github.cb372"   % "sbt-explicit-dependencies" % "0.2.16",
  "pl.project13.scala" % "sbt-jmh"                   % "0.4.3",
  "org.scalameta"      % "sbt-scalafmt"              % "2.4.3",
  "ch.epfl.scala"      % "sbt-scalafix"              % "0.9.29",
  "org.wartremover"    % "sbt-wartremover"           % "2.4.16",
  "com.github.sbt"   % "sbt-native-packager"       % "1.9.0",
  "com.eed3si9n"       % "sbt-buildinfo"             % "0.10.0",
  "com.geirsson"       % "sbt-ci-release"            % "1.5.7",
  "net.bzzt"           % "sbt-reproducible-builds"   % "0.25"
).map(addSbtPlugin)
