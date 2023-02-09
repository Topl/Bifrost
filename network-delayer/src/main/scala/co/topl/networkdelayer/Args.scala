package co.topl.networkdelayer

import cats.Show
import co.topl.common.application.{ContainsDebugFlag, ContainsUserConfigs}
import mainargs._

@main
case class Args(startup: Args.Startup)

object Args {

  @main
  case class Startup(
    @arg(
      doc = "Zero or more config files (.conf, .json, .yaml) to apply to the node." +
        "  Config files stack such that the last config file takes precedence." +
        "  To specify an internal resource, prefix the value with \"resource://\"."
    )
    config: List[String] = Nil,
    @arg(
      doc = "An optional flag to enable debug mode on this node."
    )
    debug: Flag
  )

  implicit val parserStartupArgs: ParserForClass[Args.Startup] =
    ParserForClass[Args.Startup]

  implicit val parserArgs: ParserForClass[Args] =
    ParserForClass[Args]

  implicit val argsContainsUserConfigs: ContainsUserConfigs[Args] =
    _.startup.config

  implicit val argsContainsDebugFlag: ContainsDebugFlag[Args] =
    _.startup.debug.value

  implicit val showArgs: Show[Args] =
    Show.fromToString
}
