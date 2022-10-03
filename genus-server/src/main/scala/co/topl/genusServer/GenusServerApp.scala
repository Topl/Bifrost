package co.topl.genusServer

import cats.effect._
import cats.Monad
import cats.syntax.all._
import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j._

/**
 * This will be the Genus server.
 */
object GenusServerApp extends IOApp {
  implicit val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger

  def run(args: List[String]): IO[ExitCode] = {
    println("Hello world from Genus server!")
    logger.info("Hello world from Genus server!")
    println("after logging")
    //println(s"BuildInfo: ${co.topl.buildinfo.genusServer.BuildInfo.toString}")
    IO.pure(ExitCode.Success)
  }
}
