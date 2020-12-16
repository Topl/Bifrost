import akka.actor.{ActorRef, ActorSystem, DeadLetter, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.pattern.ask
import akka.util.Timeout
import crypto.Address
import http.{GjallarhornBifrostApiRoute, GjallarhornOnlyApiRoute, HttpService, KeyManagementApi}
import keymanager.KeyManager.{ChangeNetwork, GenerateKeyFile}
import keymanager.KeyManagerRef
import requests.{ApiRoute, Requests, RequestsManager}
import settings.{AppSettings, StartupOpts}
import utils.Logging
import wallet.{DeadLetterListener, WalletManager}
import wallet.WalletManager.{GetNetwork, GjallarhornStarted, GjallarhornStopped, KeyManagerReady}

import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

class GjallarhornApp(startupOpts: StartupOpts) extends Logging with Runnable {

  private val settings: AppSettings = AppSettings.read(startupOpts)
  implicit val system: ActorSystem = ActorSystem("Gjallarhorn")
  implicit val context: ExecutionContextExecutor = system.dispatcher
  implicit val timeout: Timeout = 10.seconds

  log.info(s"${Console.MAGENTA} Gjallarhorn running in offline mode.")

  //TODO: this is the default network - but user can change this.
  //implicit val networkPrefix: NetworkPrefix = 48.toByte

  //sequence of actors for cleanly shutting down the application
  private var actorsToStop: Seq[ActorRef] = Seq()

  //Set up keyManager
  private val keyFileDir: String = settings.application.keyFileDir
  val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", keyFileDir)

  //TODO: this is just for testing purposes - should grabs keys from database
  val pk1: Address = Await.result((keyManagerRef ? GenerateKeyFile("password", Some("test")))
    .mapTo[Try[Address]], 10.seconds) match {
    case Success(pubKey) => pubKey
    case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }

  var apiRoutes: Seq[ApiRoute] = Seq(
    GjallarhornOnlyApiRoute(settings, keyManagerRef),
    KeyManagementApi(settings, keyManagerRef)
  )

  if (settings.application.chainProvider != "") {
    log.info("gjallarhorn attempting to run in online mode. Trying to connect to Bifrost...")
    val listener: ActorRef = system.actorOf(Props[DeadLetterListener]())
    actorsToStop = actorsToStop :+ listener
    system.eventStream.subscribe(listener, classOf[DeadLetter])
    system.actorSelection(s"akka.tcp://${settings.application.chainProvider}/user/walletConnectionHandler")
      .resolveOne().onComplete {
      case Success(actor) =>
        log.info(s"${Console.MAGENTA} Bifrst actor ref was found: $actor")
        setUpOnlineMode(actor)
      case Failure(ex) =>
        log.error(s"bifrost actor ref not found at: akka.tcp://${settings.application.chainProvider}. " +
          s"Continuing to run in offline mode.")
        setUpHttp()
    }
  }

  def setUpOnlineMode(bifrost: ActorRef) (implicit context: ExecutionContextExecutor, system: ActorSystem): Unit = {
    //Set up wallet manager - handshake w Bifrost and get NetworkPrefix
    val walletManagerRef: ActorRef = system.actorOf(
      Props(new WalletManager(bifrost)), name = "WalletManager")
    walletManagerRef ! GjallarhornStarted

    val bifrostResponse = Await.result((walletManagerRef ? GetNetwork).mapTo[String], 10.seconds)
    log.info(bifrostResponse)
    val networkName = bifrostResponse.split("Bifrost is running on").tail.head.replaceAll("\\s", "")
    keyManagerRef ! ChangeNetwork(networkName)
    walletManagerRef ! KeyManagerReady(keyManagerRef)
    val requestsManagerRef: ActorRef = system.actorOf(Props(new RequestsManager(bifrost)), name = "RequestsManager")
    val requests: Requests = new Requests(settings.application, requestsManagerRef, keyManagerRef)

    actorsToStop = actorsToStop ++ Seq(walletManagerRef, requestsManagerRef, keyManagerRef)
    apiRoutes = apiRoutes :+
      GjallarhornBifrostApiRoute(settings, keyManagerRef, requestsManagerRef, walletManagerRef, requests)
    setUpHttp()
  }

  def setUpHttp (): Unit = {
    //hook for initiating the shutdown procedure
    sys.addShutdownHook(GjallarhornApp.shutdown(system, actorsToStop))

    val httpService: HttpService = HttpService(apiRoutes, settings.rpcApi)
    val httpHost: String = settings.rpcApi.bindAddress.getHostName
    val httpPort: Int = settings.rpcApi.bindAddress.getPort

    Http().newServerAt(httpHost, httpPort).bind(httpService.compositeRoute).onComplete {
      case Success(serverBinding) =>
        log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")

      case Failure(ex) =>
        log.error(s"${Console.YELLOW}Failed to bind to localhost:$httpPort. " +
          s"Terminating application!${Console.RESET}", ex)
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

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

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
