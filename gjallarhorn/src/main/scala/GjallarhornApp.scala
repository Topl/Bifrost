import akka.actor.{ActorRef, ActorSystem, DeadLetter, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import http.GjallarhornApiRoute
import keymanager.{KeyManagerRef, Keys}
import requests.{Requests, RequestsManager}
import settings.{AppSettings, StartupOpts}
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
  val keyManager: Keys = Keys(keyFileDir)

  val listener: ActorRef = system.actorOf(Props[DeadLetterListener]())
  system.eventStream.subscribe(listener, classOf[DeadLetter])

  if (settings.chainProvider == "") {
    log.info("gjallarhorn running in offline mode.")
  }else{
    log.info("gjallarhorn running in online mode. Trying to connect to Bifrost...")
    system.actorSelection(s"akka.tcp://${settings.chainProvider}/user/walletConnectionHandler").resolveOne().onComplete {
      case Success(actor) =>
        log.info(s"bifrst actor ref was found: $actor")
        setUpOnlineMode(actor)
      case Failure(ex) =>
        log.error(s"bifrost actor ref not found at: akka.tcp://${settings.chainProvider}. Terminating application!${Console.RESET}")
        GjallarhornApp.shutdown(system, Seq(keyManagerRef))
    }
  }

  //sequence of actors for cleanly shutting down the application
   private val actorsToStop: Seq[ActorRef] = Seq(keyManagerRef)

  //hook for initiating the shutdown procedure
  sys.addShutdownHook(GjallarhornApp.shutdown(system, actorsToStop))

  def setUpOnlineMode(bifrost: ActorRef): Unit = {
    val walletManagerRef: ActorRef = system.actorOf(Props(new WalletManager(keyManager.listOpenKeyFiles, bifrost)), name = "WalletManager")
    val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrost)), name = "RequestsManager")
    actorsToStop ++ Seq(walletManagerRef, requestsManagerRef)
    walletManagerRef ! GjallarhornStarted
    val requests: Requests = new Requests(settings, requestsManagerRef)
    val apiRoute: Route = GjallarhornApiRoute(settings, keyManagerRef, requestsManagerRef, requests).route

    val httpPort: Int = settings.rpcPort

    Http().newServerAt("localhost", httpPort).bind(apiRoute).onComplete {
      case Success(serverBinding) =>
        log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")

      case Failure(ex) =>
        log.error(s"${Console.YELLOW}Failed to bind to localhost:$httpPort. Terminating application!${Console.RESET}", ex)
        GjallarhornApp.shutdown(system, actorsToStop)
    }
  }

  def run(): Unit = {

  }
}

object GjallarhornApp extends Logging {

  implicit val timeout: Timeout = 10.seconds

  def main(args: Array[String]): Unit = {
    new GjallarhornApp(StartupOpts.empty).run()
  }

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    val wallet: Seq[ActorRef] = actors.filter(actor => actor.path.name == "WalletManager")
    if (wallet.nonEmpty) {
      log.info ("Telling Bifrost that this wallet is shutting down.")
      val walletManager: ActorRef = wallet.head
      walletManager ! GjallarhornStopped
    }
    log.warn("Terminating Actors")
    actors.foreach { a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")

  }
}
