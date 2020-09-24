package bifrost

import java.lang.management.ManagementFactory

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.http.scaladsl.Http
import akka.io.Tcp
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import bifrost.consensus.ForgerRef
import bifrost.crypto.PrivateKey25519
import bifrost.history.History
import bifrost.http.HttpService
import bifrost.http.api.ApiRoute
import bifrost.http.api.routes._
import bifrost.mempool.MemPool
import bifrost.modifier.block.Block
import bifrost.modifier.box.Box
import bifrost.modifier.box.proposition.ProofOfKnowledgeProposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.network._
import bifrost.network.message._
import bifrost.nodeView.{NodeViewHolder, NodeViewHolderRef}
import bifrost.settings.{AppSettings, BifrostContext, NetworkType, StartupOpts}
import bifrost.utils.Logging
import com.sun.management.{HotSpotDiagnosticMXBean, VMOption}
import com.typesafe.config.{Config, ConfigFactory}
import kamon.Kamon

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

class BifrostApp(startupOpts: StartupOpts) extends Logging with Runnable {

  // todo: JAA - 2020.08.27 - We aren't using these anywhere currently. We could use an dependency injection pattern
  // todo:       and try to have this be where we define concrete type for the application, or we could remove.
  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = Box
  type TX = Transaction
  type PMOD = Block
  type NVHT = NodeViewHolder

  // Setup settings file to be passed into the application
  private val settings: AppSettings = AppSettings.read(startupOpts)
  log.debug(s"Starting application with settings \n$settings")

  // Setup name limit defined in application.conf
  private val conf: Config = ConfigFactory.load("application")
  private val ApplicationNameLimit: Int = conf.getInt("app.applicationNameLimit")

  // check for gateway device and setup port forwarding
  private val upnpGateway: Option[upnp.Gateway] = if (settings.network.upnpEnabled) upnp.Gateway(settings.network) else None

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Setup the execution environment for running the application
  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  private implicit val timeout: Timeout = Timeout(settings.network.controllerTimeout.getOrElse(5 seconds))
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  // save environment into a variable for reference throughout the application
  protected val bifrostContext = new BifrostContext(settings, upnpGateway)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Create Bifrost singleton actors
  private val peerManagerRef: ActorRef = peer.PeerManagerRef("peerManager", settings.network, bifrostContext)

  private val networkControllerRef: ActorRef = NetworkControllerRef("networkController", settings.network, peerManagerRef, bifrostContext)

  private val peerSynchronizer: ActorRef = peer.PeerSynchronizerRef("PeerSynchronizer", networkControllerRef, peerManagerRef, settings.network, bifrostContext)

  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, bifrostContext)

  private val forgerRef: ActorRef = ForgerRef("forger", nodeViewHolderRef, settings.forgingSettings, bifrostContext)

  private val nodeViewSynchronizer: ActorRef = NodeViewSynchronizerRef[Transaction, BifrostSyncInfo, Block, History, MemPool](
      "nodeViewSynchronizer", networkControllerRef, nodeViewHolderRef, settings.network, bifrostContext)

  // Sequence of actors for cleanly shutting now the application
  private val actorsToStop: Seq[ActorRef] = Seq(
    peerManagerRef,
    networkControllerRef,
    peerSynchronizer,
    nodeViewSynchronizer,
    forgerRef,
    nodeViewHolderRef
  )

  // hook for initiating the shutdown procedure
  sys.addShutdownHook(BifrostApp.shutdown(actorSystem, actorsToStop))

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Create and register controllers for API routes
  private val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings),
    AssetApiRoute(settings, nodeViewHolderRef),
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ProgramApiRoute(settings, nodeViewHolderRef),
    NodeViewApiRoute(settings, nodeViewHolderRef)
  )

  private val httpService = HttpService(apiRoutes)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Am I running on a JDK that supports JVMCI?
  val vm_version: String = System.getProperty("java.vm.version")
  System.out.printf("java.vm.version = %s%n", vm_version)

  // Is JVMCI enabled?
  val bean: HotSpotDiagnosticMXBean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
  val enableJVMCI: VMOption = bean.getVMOption("EnableJVMCI")
  System.out.println(enableJVMCI)

  // Is the system using the JVMCI compiler for normal compilations?
  val useJVMCICompiler: VMOption = bean.getVMOption("UseJVMCICompiler")
  System.out.println(useJVMCICompiler)

  // What compiler is selected?
  val compiler: String = System.getProperty("jvmci.Compiler")
  System.out.printf("jvmci.Compiler = %s%n", compiler)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  def run(): Unit = {
    require(settings.network.agentName.length <= ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at 0.0.0.0:${settings.rpcPort}")

    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val httpHost = "0.0.0.0"
    val httpPort = settings.rpcPort

    def failedP2P(): Unit = {
      log.error(s"${Console.RED}Unable to bind to the P2P port. Terminating application!${Console.RESET}")
      BifrostApp.shutdown(actorSystem, actorsToStop)
    }

    // trigger the P2P network bind and check that the protocol bound successfully. Terminate the application on failure
    (networkControllerRef ? NetworkController.ReceivableMessages.BindP2P).onComplete {
      case Success(bindResponse: Future[Any]) =>
        bindResponse.onComplete({
          case Success(Tcp.Bound(addr)) =>
            log.info(s"${Console.YELLOW}P2P protocol bound to ${addr}${Console.RESET}")
            networkControllerRef ! NetworkController.ReceivableMessages.BecomeOperational

          case Success(_) | Failure(_) => failedP2P()
        })
      case Success(_) | Failure(_) => failedP2P()
    }

    // trigger the HTTP server bind and check that the bind is successful. Terminate the application on failure
    Http().newServerAt(httpHost, httpPort).bind(httpService.compositeRoute).onComplete {
      case Success(serverBinding) =>
        log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")

      case Failure(ex) =>
        log.error(s"${Console.YELLOW}Failed to bind to $httpHost:$httpPort. Terminating application!${Console.RESET}", ex)
        BifrostApp.shutdown(actorSystem, actorsToStop)
    }

  }
}

// This is the primary application object and is the entry point for Bifrost to begin execution
object BifrostApp extends Logging {
  // check if Kamon instrumentation should be started.
  // DO NOT MOVE!! This must happen before anything else!
  private val conf: Config = ConfigFactory.load("application")
  if (conf.getBoolean("kamon.enable")) Kamon.init()

  import com.joefkelley.argyle._ // import for parsing command line arguments

  // parse command line arguments
  val argParser: Arg[StartupOpts] = (
    optional[String]("--config", "-c") and
      optionalOneOf[NetworkType](NetworkType.all.map(x => s"--${x.verboseName}" -> x): _*)
    ).to[StartupOpts]

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  def main(args: Array[String]): Unit =
    argParser.parse(args) match {
      case Success(argsParsed) => new BifrostApp(argsParsed).run()
      case Failure(e) => throw e
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