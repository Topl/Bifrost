package co.topl.genusServer

import cats.effect._
import co.topl.genusLibrary.Genus
import org.typelevel.log4cats._
import org.typelevel.log4cats.slf4j._

import scala.annotation.unused

/**
 * This will be the Genus server.
 */
object GenusServerApp extends IOApp {
  implicit val logger: SelfAwareStructuredLogger[IO] = LoggerFactory[IO].getLogger

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- logger.info("Genus server starting")
      _ <- doIt(args).handleErrorWith(e => logger.info(e)("Genus server terminating with unexpected error"))
      _ <- logger.info("Exiting Genus server")
      // println(s"BuildInfo: ${co.topl.buildinfo.genusServer.BuildInfo.toString}")
    } yield ExitCode.Success

  def doIt(@unused args: List[String]): IO[Unit] =
    IO {
      try {
        Genus.getGenus
        // Code to run gRPC services goes here
      } finally {
        Genus.shutDown()
      }
    }
}
