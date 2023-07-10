package co.topl.node

import cats.Show
import co.topl.common.application.{ContainsDebugFlag, ContainsUserConfigs}
import mainargs._
import monocle.macros.GenLens
import monocle.macros.Lenses
import monocle.syntax.all._

// $COVERAGE-OFF$

@main @Lenses
case class Args(startup: Args.Startup, runtime: Args.Runtime)

object Args {

  @main @Lenses
  case class Startup(
    @arg(
      doc = "Zero or more config files (.conf, .json, .yaml) to apply to the node." +
        "  Config files stack such that the last config file takes precedence." +
        "  To specify an internal resource, prefix the value with \"resource://\"."
    )
    config: Seq[String] = Nil,
    @arg(
      doc = "An optional path to a logback.xml file to override the logging configuration of the node."
    )
    logbackFile: Option[String] = None,
    @arg(
      doc = "An optional flag to enable debug mode on this node."
    )
    debug: Flag,
    cli:   Boolean = false
  )

  @main @Lenses
  case class Runtime(
    @arg(
      doc = "The directory to use when saving/reading blockchain data"
    )
    dataDir: Option[String],
    @arg(
      doc = "The directory of the block producer's staking keys"
    )
    stakingDir: Option[String],
    @arg(
      doc = "The hostname to bind to for the RPC layer (i.e. localhost or 0.0.0.0)"
    )
    rpcBindHost: Option[String] = None,
    @arg(
      doc = "The port to bind to for the RPC layer (i.e. 9085)"
    )
    rpcBindPort: Option[Int] = None,
    @arg(
      doc = "The hostname to bind to for the P2P layer (i.e. localhost or 0.0.0.0)"
    )
    p2pBindHost: Option[String] = None,
    @arg(
      doc = "The port to bind to for the P2P layer (i.e. 9084)"
    )
    p2pBindPort: Option[Int] = None,
    @arg(
      doc = "A comma-delimited list of host:port values to connect to at launch (i.e. 1.2.3.4:9084,5.6.7.8:9084)"
    )
    knownPeers:  Option[String] = None,
    testnetArgs: PrivateTestnetArgs,
    genusArgs:   GenusArgs
  )

  @main @Lenses
  case class PrivateTestnetArgs(
    @arg(
      doc = "A UTC Unix epoch timestamp (ms) to use when seeding a private testnet."
    )
    testnetTimestamp: Option[Long] = None,
    @arg(
      doc = "The number of stakers to initialize."
    )
    testnetStakerCount: Option[Int] = None,
    @arg(
      doc = "The index of the staker to launch."
    )
    testnetStakerIndex: Option[Int] = None
  )

  @main @Lenses
  case class GenusArgs(
    @arg(
      doc = "Disables the Genus server and Genus gRPC services"
    )
    disableGenus: Flag,
    @arg(
      doc = "The directory to use when saving/reading graph data"
    )
    orientDbDir: Option[String],
    @arg(
      doc = "The password to use when connecting to OrientDB"
    )
    orientDbPassword: Option[String]
  )

  def parse(args: Seq[String]): Args =
    if (args.headOption.contains("cli")) parserArgs.constructOrThrow(args.tail).focus(_.startup.cli).replace(true)
    else parserArgs.constructOrThrow(args)

  implicit val showArgs: Show[Args] = {
    val base = Show.fromToString[Args]
    val genLens = GenLens[Args]
    val sanitizer = genLens(_.runtime.genusArgs.orientDbPassword).replace(Some("SANITIZED"))
    args => base.show(sanitizer(args))
  }

  implicit val parserPrivateTestnetArgs: ParserForClass[PrivateTestnetArgs] =
    ParserForClass[PrivateTestnetArgs]

  implicit val parserGenusArgs: ParserForClass[GenusArgs] =
    ParserForClass[GenusArgs]

  implicit val parserStartup: ParserForClass[Startup] =
    ParserForClass[Startup]

  implicit val parserRuntime: ParserForClass[Runtime] =
    ParserForClass[Runtime]

  implicit val parserArgs: ParserForClass[Args] =
    ParserForClass[Args]

  implicit val argsContainsUserConfigs: ContainsUserConfigs[Args] =
    _.startup.config.toList

  implicit val argsContainsDebugFlag: ContainsDebugFlag[Args] =
    _.startup.debug.value
}
// $COVERAGE-ON$
