import akka.actor.{ActorRef, ActorSystem, DeadLetter, PoisonPill, Props}
import akka.pattern.ask
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import http.GjallarhornApiRoute
import keymanager.{KeyManagerRef, Keys}
import requests.{Requests, RequestsManager}
import settings.{AppSettings, NetworkType, StartupOpts}
import utils.Logging
import wallet.{DeadLetterListener, WalletManager}
import wallet.WalletManager.{GjallarhornStarted, GjallarhornStopped}

import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.{Failure, Success}
import scala.concurrent.duration._

class GjallarhornApp(startupOpts: StartupOpts) extends Logging with Runnable {

  private val settings: AppSettings = AppSettings.read(startupOpts)
  implicit val system: ActorSystem = ActorSystem("Gjallarhorn")
  implicit val context: ExecutionContextExecutor = system.dispatcher
  implicit val timeout: Timeout = 10.seconds

  private val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", "keyfiles")
  val keyFileDir: String = settings.keyFileDir
  val keyManager: Keys = Keys(Set(), keyFileDir)

  val walletManagerRef: ActorRef = system.actorOf(Props(new WalletManager(keyManager.listOpenKeyFiles)), name = "WalletManager")
  val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager), name = "RequestsManager")

  val listener = system.actorOf(Props[DeadLetterListener]())
  system.eventStream.subscribe(listener, classOf[DeadLetter])

  if (settings.chainProvider == "") {
    log.info("gjallarhorn running in offline mode.")
  }else{
    log.info("gjallarhorn running in online mode. Trying to connect to Bifrost...")
    system.actorSelection(s"akka.tcp://${settings.chainProvider}/user/walletConnectionHandler").resolveOne().onComplete {
      case Success(actor) => {
        log.info(s"bifrst actor ref was found: $actor")
        walletManagerRef ! GjallarhornStarted(actor)
      }
      case Failure(ex) =>
        log.error(s"bifrost actor ref not found at: akka.tcp://${settings.chainProvider}. Terminating application!${Console.RESET}")
        GjallarhornApp.shutdown(system, Seq(keyManagerRef))
    }
  }

  //sequence of actors for cleanly shutting down the application
   private val actorsToStop: Seq[ActorRef] = Seq(
     keyManagerRef,
     walletManagerRef,
     requestsManagerRef
   )

  //hook for initiating the shutdown procedure
  sys.addShutdownHook(GjallarhornApp.shutdown(system, actorsToStop))

  val requests: Requests = new Requests(settings, requestsManagerRef)
  val httpPort: Int = settings.rpcPort

  private val apiRoute: Route = GjallarhornApiRoute(settings, keyManagerRef, requestsManagerRef, requests).route

  Http().newServerAt("localhost", httpPort).bind(apiRoute).onComplete {
    case Success(serverBinding) =>
      log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")

    case Failure(ex) =>
      log.error(s"${Console.YELLOW}Failed to bind to localhost:$httpPort. Terminating application!${Console.RESET}", ex)
      GjallarhornApp.shutdown(system, actorsToStop)
  }


  def run(): Unit = {

  }
}

object GjallarhornApp extends Logging {

  import com.joefkelley.argyle._

  implicit val timeout: Timeout = 10.seconds

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

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    val wallet: Seq[ActorRef] = actors.filter(actor => actor.path.name == "WalletManager")
    if (wallet.nonEmpty) {
      log.info ("Telling Bifrost that this wallet is shutting down.")
      val walletManager: ActorRef = wallet.head
      val bifrostResponse: String = Await.result((walletManager ? GjallarhornStopped).mapTo[String], 100.seconds)
      log.info(s"$bifrostResponse")
    }
    log.warn("Terminating Actors")
    actors.foreach { a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")

  }
}
