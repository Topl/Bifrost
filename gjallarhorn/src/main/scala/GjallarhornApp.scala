import akka.actor.{ActorRef, ActorSystem, DeadLetter, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.pattern.ask
import akka.util.Timeout
import crypto.Address
import io.circe.syntax._
import http.{GjallarhornBifrostApiRoute, GjallarhornOnlyApiRoute, HttpService, KeyManagementApi}
import io.circe.Json
import keymanager.KeyManager.GenerateKeyFile
import keymanager.KeyManagerRef
import requests.{ApiRoute, Requests}
import settings.{AppSettings, StartupOpts}
import utils.Logging
import wallet.DeadLetterListener

import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

class GjallarhornApp(startupOpts: StartupOpts) extends Logging with Runnable {

  private val settings: AppSettings = AppSettings.read(startupOpts)
  implicit val system: ActorSystem = ActorSystem("Gjallarhorn")
  implicit val context: ExecutionContextExecutor = system.dispatcher
  implicit val timeout: Timeout = 10.seconds

  log.info(s"${Console.MAGENTA} Gjallarhorn running in offline mode.${Console.RESET}")

  //sequence of actors for cleanly shutting down the application
  private var actorsToStop: Seq[ActorRef] = Seq()

  //Set up keyManager
  private val keyFileDir: String = settings.application.keyFileDir
  val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", keyFileDir)
  val requests: Requests = new Requests(settings.application, keyManagerRef)

  //TODO: this is just for testing purposes - should grabs keys from database
  val pk1: Address = Await.result((keyManagerRef ? GenerateKeyFile("password", Some("test")))
    .mapTo[Try[Address]], 10.seconds) match {
    case Success(pubKey) => pubKey
    case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }

  val gjalBifrostRoute: ApiRoute = GjallarhornBifrostApiRoute(settings, keyManagerRef, requests)

  var apiRoutes: Seq[ApiRoute] = Seq(
    GjallarhornOnlyApiRoute(settings, keyManagerRef),
    KeyManagementApi(settings, keyManagerRef),
    gjalBifrostRoute
  )

  val connectRequest: Vector[Json] = Vector(Map("params" ->
    Vector(Map("chainProvider" -> settings.application.chainProvider).asJson)).asJson)

  gjalBifrostRoute.handlers("onlineWallet_connectToBifrost", connectRequest, "2")

  val listener: ActorRef = system.actorOf(Props[DeadLetterListener]())
  actorsToStop = actorsToStop ++ Seq(listener, keyManagerRef)
  system.eventStream.subscribe(listener, classOf[DeadLetter])

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
    log.warn("Terminating Actors")
    actors.foreach { a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }
}
