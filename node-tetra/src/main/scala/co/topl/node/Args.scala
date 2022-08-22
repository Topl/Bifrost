package co.topl.node

import cats.Show
import mainargs._

@main
case class Args()

object Args {

  implicit val showArgs: Show[Args] =
    Show.show(args => "Args()")

  implicit val parserArgs: ParserForClass[Args] =
    ParserForClass[Args]
}
