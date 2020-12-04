import akka.actor.{ActorRef, ActorSystem, DeadLetter, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import crypto.AddressEncoder.NetworkPrefix
import crypto.{KeyfileCurve25519, PrivateKeyCurve25519}
import http.{GjallarhornApiRoute, HttpService}
import keymanager.{KeyManagerRef, Keys}
import requests.{ApiRoute, Requests, RequestsManager}
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
  //TODO: how to get network prefix?
  implicit val networkPrefix: NetworkPrefix = 1.toByte
  implicit val timeout: Timeout = 10.seconds

  val keyFileDir: String = settings.application.keyFileDir
  val keyManager: Keys[PrivateKeyCurve25519, KeyfileCurve25519] = Keys(keyFileDir, KeyfileCurve25519)
  private val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", keyManager)

  val listener: ActorRef = system.actorOf(Props[DeadLetterListener]())
  system.eventStream.subscribe(listener, classOf[DeadLetter])

  if (settings.application.chainProvider == "") {
    log.info("gjallarhorn running in offline mode.")
  }else{
    log.info("gjallarhorn running in online mode. Trying to connect to Bifrost...")
    system.actorSelection(s"akka.tcp://${settings.application.chainProvider}/user/walletConnectionHandler").resolveOne().onComplete {
      case Success(actor) =>
        log.info(s"bifrst actor ref was found: $actor")
        setUpOnlineMode(actor)
      case Failure(ex) =>
        log.error(s"bifrost actor ref not found at: akka.tcp://${settings.application.chainProvider}. Terminating application!${Console.RESET}")
        GjallarhornApp.shutdown(system, Seq(keyManagerRef))
    }
  }

  //sequence of actors for cleanly shutting down the application
   private val actorsToStop: Seq[ActorRef] = Seq(keyManagerRef)

  //hook for initiating the shutdown procedure
  sys.addShutdownHook(GjallarhornApp.shutdown(system, actorsToStop))

  def setUpOnlineMode(bifrost: ActorRef): Unit = {
    val walletManagerRef: ActorRef = system.actorOf(Props(new WalletManager(keyManager.addresses, bifrost)), name = "WalletManager")
    val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrost)), name = "RequestsManager")
    actorsToStop ++ Seq(walletManagerRef, requestsManagerRef)
    walletManagerRef ! GjallarhornStarted
    val requests: Requests = new Requests(settings.application, requestsManagerRef)
    val apiRoute: ApiRoute = GjallarhornApiRoute(settings, keyManagerRef, requestsManagerRef, requests)

    val httpService = HttpService(Seq(apiRoute), settings.rpcApi)
    val httpHost = settings.rpcApi.bindAddress.getHostName
    val httpPort: Int = settings.rpcApi.bindAddress.getPort

    Http().newServerAt(httpHost, httpPort).bind(httpService.compositeRoute).onComplete {
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
