package co.topl

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.effect.IO
import cats.implicits._
import co.topl.catsakka.IOAkkaApp
import com.typesafe.config.ConfigFactory
import mainargs.ParserForClass
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object NodeApp
    extends IOAkkaApp[NodeAppCommandLineArgs, Nothing](
      createArgs = args => ParserForClass[NodeAppCommandLineArgs].constructOrThrow(args),
      createConfig = args => ConfigFactory.load(),
      createSystem = (args, config) => ActorSystem[Nothing](Behaviors.empty, "BifrostTetra", config)
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  def run: IO[Unit] =
    for {
      _ <- Logger[F].info(show"Launching node with args=$args")
      _ <- Logger[F].info("Running forever...")
      _ <- IO.never[Nothing]
    } yield ()
}
