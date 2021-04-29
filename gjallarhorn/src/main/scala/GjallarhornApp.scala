import akka.actor.{ActorRef, ActorSystem, DeadLetter, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.pattern.ask
import akka.util.Timeout
import attestation.Address
import io.circe.syntax._
import http.{GjallarhornOfflineApiRoute, GjallarhornOnlineApiRoute, HttpService, KeyManagementApiRoute}
import io.circe.Json
import keymanager.KeyManager.GenerateKeyFile
import keymanager.KeyManagerRef
import requests.{ApiRoute, Requests}
import settings.{AppSettings, StartupOpts}
import utils.Logging
import wallet.{DeadLetterListener, WalletManager}

import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._
import scala.reflect.io.Path

/**
 * Gjallarhorn is the wallet application for Bifrost, the Topl blockchain.
 * @param startupOpts optional parameters for the application start up
 */
class GjallarhornApp(startupOpts: StartupOpts) extends Logging with Runnable {

  // Setup settings file to be passed into the application
  private val settings: AppSettings = AppSettings.read(startupOpts)

  //Setup the execution environment for running the application
  implicit val system: ActorSystem = ActorSystem("Gjallarhorn")
  implicit val context: ExecutionContextExecutor = system.dispatcher
  implicit val timeout: Timeout = 10.seconds

  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */
  //Initially set-up offline mode
  log.info(s"${Console.MAGENTA} Gjallarhorn running in offline mode.${Console.RESET}")

  //Set up keyManager
  //TODO: won't actually want to delete old keys - this is for testing purposes:
  val path: Path = Path(settings.application.keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", settings.application)

  //TODO: this is just for testing purposes - shouldn't create keys on start-up
  val pk1: Address = Await.result(
    (keyManagerRef ? GenerateKeyFile("password", Some("test")))
      .mapTo[Try[Address]],
    10.seconds
  ) match {
    case Success(pubKey) => pubKey
    case Failure(ex)     => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }

  //Create WalletManager actor
  val walletManagerRef: ActorRef =
    system.actorOf(Props(new WalletManager(keyManagerRef)), name = WalletManager.actorName)

  //Create requests object
  val requests: Requests = new Requests(settings, keyManagerRef)

  //Set up API routes
  val gjalBifrostRoute: ApiRoute =
    GjallarhornOnlineApiRoute(settings.rpcApi, settings.application, keyManagerRef, walletManagerRef, requests)

  val apiRoutes: Seq[ApiRoute] = Seq(
    GjallarhornOfflineApiRoute(settings.rpcApi, settings.application, keyManagerRef, walletManagerRef),
    KeyManagementApiRoute(settings.rpcApi, keyManagerRef),
    gjalBifrostRoute
  )

  //Attempt to connect to Bifrost and start online mode.
  val connectRequest: Vector[Json] = Vector(
    Map(
      "params" ->
      Vector(
        Map(
          "chainProvider" -> settings.application.defaultChainProviders
            .get(settings.application.currentChainProvider)
        ).asJson
      )
    ).asJson
  )
  try gjalBifrostRoute.handlers("onlineWallet_connectToBifrost", connectRequest, "2")
  catch {
    case _: Exception => log.warn(s"${Console.RED} Continuing to run in offline mode. ${Console.RESET}")
  }

  //DeadLetter listener set up for debugging purposes
  val listener: ActorRef = system.actorOf(Props[DeadLetterListener]())
  system.eventStream.subscribe(listener, classOf[DeadLetter])

  //sequence of actors for cleanly shutting down the application
  private val actorsToStop: Seq[ActorRef] = Seq(listener, keyManagerRef, walletManagerRef)

  //hook for initiating the shutdown procedure
  sys.addShutdownHook(GjallarhornApp.shutdown(system, actorsToStop, gjalBifrostRoute))

  //Set-up http server info:
  val httpService: HttpService = HttpService(apiRoutes, settings.rpcApi)
  val httpHost: String = settings.rpcApi.bindHostname
  val httpPort: Int = settings.rpcApi.bindPort

  // trigger the HTTP server bind and check that bind was successful.
  // terminates application on failure
  Http().newServerAt(httpHost, httpPort).bind(httpService.compositeRoute).onComplete {
    case Success(serverBinding) =>
      log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")
    case Failure(ex) =>
      log.error(
        s"${Console.YELLOW}Failed to bind to localhost:$httpPort. " +
        s"Terminating application!${Console.RESET}",
        ex
      )
      GjallarhornApp.shutdown(system, actorsToStop, gjalBifrostRoute)
  }

  def run(): Unit = {}
}

object GjallarhornApp extends Logging {

  implicit val timeout: Timeout = 10.seconds

  def main(args: Array[String]): Unit =
    new GjallarhornApp(StartupOpts.empty).run()

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef], apiRoute: ApiRoute): Unit = {
    apiRoute.handlers("onlineWallet_disconnectFromBifrost", Vector("".asJson), "2")
    log.warn("Terminating Actors")
    actors.foreach(a => a ! PoisonPill)
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }
}
