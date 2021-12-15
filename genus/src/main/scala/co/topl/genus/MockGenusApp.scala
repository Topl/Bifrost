package co.topl.genus

import akka.actor.ActorSystem
import co.topl.genus.programs.GenusProgram
import com.typesafe.config.ConfigFactory

object MockGenusApp extends App {

  val conf = ConfigFactory
    .parseString("akka.http.server.preview.enable-http2 = on")
    .withFallback(ConfigFactory.defaultApplication())

  val system = ActorSystem("genus", conf)

  val program =
    GenusProgram.Mock.make(
      system,
      "127.0.0.1",
      8080
    )

  while (true) {}
}
