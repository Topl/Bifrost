package co.topl.tools.exporter

import mainargs.{main, ParserForMethods}

object Exporter {

  @main
  def mongo(): Unit = {}

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)

}
