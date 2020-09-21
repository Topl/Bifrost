import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import http.GjallarhornApiRoute
import keymanager.KeyManagerRef
import settings.{AppSettings, NetworkType, StartupOpts}
import utils.Logging

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

class GjallarhornApp(startupOpts: StartupOpts) extends Runnable {

  implicit val system: ActorSystem = ActorSystem("Gjallarhorn")
  implicit val context: ExecutionContextExecutor = system.dispatcher

  private val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", "keyfiles")
  private val settings: AppSettings = AppSettings.read(startupOpts)

  val httpPort: Int = settings.rpcPort

  private val apiRoute: Route = GjallarhornApiRoute(keyManagerRef).route
  Http().newServerAt("localhost", httpPort).bind(apiRoute)

  def run(): Unit = {

  }
}

object GjallarhornApp extends Logging {

  import com.joefkelley.argyle._

  val argParser: Arg[StartupOpts] = (
    optional[String]("--config", "-c") and
      optionalOneOf[NetworkType](NetworkType.all.map(x => s"--${x.verboseName}" -> x): _*)
    ).to[StartupOpts]

  def main(args: Array[String]): Unit = {
    argParser.parse(args) match {
      case Success(argsParsed) => new GjallarhornApp(argsParsed).run()
      case Failure(e) => throw e
    }
  }
}
