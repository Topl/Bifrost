// Comment to get more information during initialization
logLevel := Level.Error

Seq(
  "com.eed3si9n"       % "sbt-assembly"              % "0.15.0",
  "org.scalastyle"    %% "scalastyle-sbt-plugin"     % "1.0.0",
  "org.scoverage"      % "sbt-scoverage"             % "1.7.2",
  "com.github.gseitz"  % "sbt-release"               % "1.0.13",
  "net.virtual-void"   % "sbt-dependency-graph"      % "0.9.2",
  "io.kamon"           % "sbt-kanela-runner"         % "2.0.6",
  "com.github.cb372"   % "sbt-explicit-dependencies" % "0.2.11",
  "pl.project13.scala" % "sbt-jmh"                   % "0.3.7",
  "org.scalameta"      % "sbt-scalafmt"              % "2.4.0",
  "ch.epfl.scala"      % "sbt-scalafix"              % "0.9.26",
  "org.wartremover"    % "sbt-wartremover"           % "2.4.13",
  "com.typesafe.sbt"   % "sbt-native-packager"       % "1.7.6",
  "com.eed3si9n"       % "sbt-buildinfo"             % "0.10.0",
  "com.geirsson"       % "sbt-ci-release"            % "1.5.7"
).map(addSbtPlugin)
