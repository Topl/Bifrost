package co.topl.genus

import cats.implicits._
import akka.actor.ActorSystem
import cats.effect.{Async, IO, IOApp}
import co.topl.genus.interpreters.GenusServer
import co.topl.genus.programs.GenusProgram
import com.typesafe.config.{Config, ConfigFactory}

object GenusApp extends IOApp.Simple {

  val config: Config =
    ConfigFactory
      .parseString("akka.http.server.preview.enable-http2 = on")
      .withFallback(ConfigFactory.defaultApplication())

  implicit val system: ActorSystem = ActorSystem("genus", config)

  val serverIp = "127.0.0.1"
  val serverPort = 8080

  override def run: IO[Unit] =
    GenusProgram.Mock
      .make[IO](
        GenusServer.Mock.make[IO](serverIp, serverPort)
      )
      .flatMap(_ => IO.never)
      .guarantee(Async[IO].fromFuture(IO.delay(system.terminate())).void)
}
