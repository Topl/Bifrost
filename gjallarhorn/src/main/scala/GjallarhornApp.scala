import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import http.GjallarhornApiRoute
import keymanager.{KeyManagerRef, Keys}
import settings.{AppSettings, NetworkType, StartupOpts}
import utils.Logging
import wallet.WalletManager

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

class GjallarhornApp(startupOpts: StartupOpts) extends Logging with Runnable {

  implicit val system: ActorSystem = ActorSystem("Gjallarhorn")
  implicit val context: ExecutionContextExecutor = system.dispatcher

  private val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", "keyfiles")
  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager = Keys(Set(), keyFileDir)

  private val walletManagerRef: ActorRef = system.actorOf(Props(new WalletManager(keyManager.listOpenKeyFiles)))

  implicit val settings: AppSettings = AppSettings.read(startupOpts)

  val httpPort: Int = settings.rpcPort

  private val apiRoute: Route = GjallarhornApiRoute(settings, keyManagerRef, walletManagerRef).route
  Http().newServerAt("localhost", httpPort).bind(apiRoute).onComplete {
    case Success(serverBinding) =>
      log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")

    case Failure(ex) =>
      log.error(s"${Console.YELLOW}Failed to bind to localhost:$httpPort. Terminating application!${Console.RESET}", ex)
  }


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
