package co.topl

import java.lang.management.ManagementFactory

import akka.actor.{ ActorRef, ActorSystem, PoisonPill }
import akka.http.scaladsl.Http
import akka.io.Tcp
import akka.pattern.ask
import akka.util.Timeout
import co.topl.consensus.{ Forger, ForgerRef }
import co.topl.http.HttpService
import co.topl.http.api.ApiRoute
import co.topl.http.api.routes._
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.network.NetworkController.ReceivableMessages.BindP2P
import co.topl.network._
import co.topl.network.message.BifrostSyncInfo
import co.topl.network.upnp.Gateway
import co.topl.nodeView.{ NodeViewHolder, NodeViewHolderRef }
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.settings.{ AppContext, AppSettings, NetworkType, RuntimeOpts, StartupOpts }
import co.topl.utils.Logging
import com.sun.management.{ HotSpotDiagnosticMXBean, VMOption }
import com.typesafe.config.{ Config, ConfigFactory }
import kamon.Kamon

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.util.{ Failure, Success }

class BifrostApp(startupOpts: StartupOpts) extends Logging with Runnable {

  type BSI = BifrostSyncInfo
  type TX = Transaction
  type PMOD = Block
  type HIS = History
  type MP = MemPool

  // Setup settings file to be passed into the application
  private val settings: AppSettings = AppSettings.read(startupOpts)
  log.debug(s"Starting application with settings \n$settings")

  // check for gateway device and setup port forwarding
  private val upnpGateway: Option[Gateway] = if (settings.network.upnpEnabled) upnp.Gateway(settings.network) else None

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Setup the execution environment for running the application
  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  private implicit val timeout: Timeout = Timeout(settings.network.controllerTimeout.getOrElse(5 seconds))
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  // save runtime environment into a variable for reference throughout the application
  protected val appContext = new AppContext(settings, startupOpts, upnpGateway)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Create Bifrost singleton actors
  private val peerManagerRef: ActorRef = PeerManagerRef(PeerManager.actorName, settings, appContext)

  private val networkControllerRef: ActorRef = NetworkControllerRef(NetworkController.actorName, settings, peerManagerRef, appContext)

  private val forgerRef: ActorRef = ForgerRef(Forger.actorName, settings, appContext)

  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef(NodeViewHolder.actorName, settings, appContext)

  private val peerSynchronizer: ActorRef = PeerSynchronizerRef(PeerSynchronizer.actorName, networkControllerRef, peerManagerRef, settings, appContext)

  private val nodeViewSynchronizer: ActorRef = NodeViewSynchronizerRef[TX, BSI, PMOD, HIS, MP](
      NodeViewSynchronizer.actorName, networkControllerRef, nodeViewHolderRef, settings, appContext)

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
    UtilsApiRoute(settings.restApi),
    KeyManagementApiRoute(settings.restApi, forgerRef),
    AssetApiRoute(settings.restApi, nodeViewHolderRef),
    DebugApiRoute(settings.restApi, nodeViewHolderRef),
    WalletApiRoute(settings.restApi, nodeViewHolderRef),
    ProgramApiRoute(settings.restApi, nodeViewHolderRef),
    NodeViewApiRoute(settings.restApi, nodeViewHolderRef)
  )

  private val httpService = HttpService(apiRoutes)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Am I running on a JDK that supports JVMCI?
  val vm_version: String = System.getProperty("java.vm.version")
  System.out.printf("java.vm.version = %s%n", vm_version)

  // Is JVMCI enabled?
  val bean: HotSpotDiagnosticMXBean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
  val enableJVMCI: VMOption = bean.getVMOption("EnableJVMCI")
  log.debug(s"$enableJVMCI")

  // Is the system using the JVMCI compiler for normal compilations?
  val useJVMCICompiler: VMOption = bean.getVMOption("UseJVMCICompiler")
  log.debug(s"$useJVMCICompiler")

  // What compiler is selected?
  val compiler: String = System.getProperty("jvmci.Compiler")
  System.out.printf("jvmci.Compiler = %s%n", compiler)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  def run(): Unit = {
    require(settings.network.agentName.length <= settings.network.applicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at: ${settings.restApi.bindAddress}")

    val httpHost = settings.restApi.bindAddress.getHostName
    val httpPort = settings.restApi.bindAddress.getPort

    /** Helper function to kill the application if needed */
    def failedP2P(): Unit = {
      log.error(s"${Console.RED}Unable to bind to the P2P port. Terminating application!${Console.RESET}")
      BifrostApp.shutdown(actorSystem, actorsToStop)
    }

    // trigger the P2P network bind and check that the protocol bound successfully. Terminate the application on failure
    (networkControllerRef ? BindP2P).onComplete {
      case Success(bindResponse: Future[Any]) =>
        bindResponse.onComplete {
          case Success(Tcp.Bound(addr)) => log.info(s"${Console.YELLOW}P2P protocol bound to $addr${Console.RESET}")
          case Success(_) | Failure(_) => failedP2P()
        }
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
  val argParser: Arg[StartupOpts] =
    (optional[String]("--config", "-c") and
      optionalOneOf[NetworkType](NetworkType.all.map(x => s"--${x.verboseName}" -> x) : _*) and
      ( optional[String]("--seed", "-s") and
        flag("--forge", "-f")
        ).to[RuntimeOpts]
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
    actors.foreach { _ ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }
}