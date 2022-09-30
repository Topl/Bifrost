package co.topl.genusServer

import cats.effect._

/**
 * This will be the Genus server.
 */
object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    println("Hello world from Genus server!")
    //println(s"BuildInfo: ${co.topl.buildinfo.genusServer.BuildInfo.toString}")
    IO.pure(ExitCode.Success)
  }
}
