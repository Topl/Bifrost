package co.topl

import cats.Show

case class NodeAppCommandLineArgs()

object NodeAppCommandLineArgs {

  implicit val showNodeAppCommandLineArgs: Show[NodeAppCommandLineArgs] =
    Show.show(args => "Args()")
}
