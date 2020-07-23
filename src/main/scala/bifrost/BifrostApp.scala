package bifrost

import java.lang.management.ManagementFactory
import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.http.scaladsl.Http
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
import bifrost.nodeView.{NodeViewHolder, NodeViewHolderRef, NodeViewModifier}
import bifrost.settings.{AppSettings, BifrostContext, NetworkType, StartupOpts}
import bifrost.utils.{Logging, NetworkTimeProvider}
import com.sun.management.{HotSpotDiagnosticMXBean, VMOption}
import com.typesafe.config.{Config, ConfigFactory}
import kamon.Kamon

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.util.{Failure, Success}

class BifrostApp(startupOpts: StartupOpts) extends Logging with Runnable {

  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = Box
  type TX = Transaction
  type PMOD = Block
  type NVHT = NodeViewHolder

  private val settings: AppSettings = AppSettings.read(startupOpts)
  log.debug(s"Starting application with settings \n$settings")

  private val conf: Config = ConfigFactory.load("application")
  private val ApplicationNameLimit: Int = conf.getInt("app.applicationNameLimit")

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Setup the execution environment for running the application
  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  private implicit val timeout: Timeout = Timeout(settings.network.controllerTimeout.getOrElse(5 seconds))
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher
  private val timeProvider = new NetworkTimeProvider(settings.ntp)

  // check for gateway device and setup port forwarding
  private val upnpGateway: Option[upnp.Gateway] = if (settings.network.upnpEnabled) upnp.Gateway(settings.network) else None

  // save your address for sending to others peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, u.mappedPort))
    }
  }

  // enumerate features and message specs present for
  protected val features: Seq[peer.PeerFeature] = Seq()
  protected val featureSerializers: peer.PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap
  protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BifrostSyncInfoMessageSpec)

  private lazy val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)
    val peersSpec = new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects)
    Seq(
      GetPeersSpec,
      peersSpec,
      invSpec,
      requestModifierSpec,
      modifiersSpec
    )
  }

  // save environment into a variable for reference throughout the application
  private val bifrostContext: BifrostContext = BifrostContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // Create Bifrost singleton actors
  private val peerManagerRef: ActorRef = peer.PeerManagerRef("peerManager", settings, bifrostContext)

  private val networkControllerRef: ActorRef = NetworkControllerRef("networkController", settings.network, peerManagerRef, bifrostContext)

  private val peerSynchronizer: ActorRef = peer.PeerSynchronizerRef("PeerSynchronizer", networkControllerRef, peerManagerRef, settings.network, featureSerializers)

  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, timeProvider)

  private val forgerRef: ActorRef = ForgerRef("forger", settings, nodeViewHolderRef)

  private val nodeViewSynchronizer: ActorRef =
    NodeViewSynchronizerRef[Transaction, BifrostSyncInfo, BifrostSyncInfoMessageSpec.type, Block, History, MemPool](
      "nodeViewSynchronizer", networkControllerRef, nodeViewHolderRef,
      BifrostSyncInfoMessageSpec, settings.network, timeProvider, NodeViewModifier.modifierSerializers)

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
  // Register controllers for all API routes
  private val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ProgramApiRoute(settings, nodeViewHolderRef),
    AssetApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
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
  def run(): Unit = {
    require(settings.network.agentName.length <= ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at 0.0.0.0:${settings.rpcPort}")

    implicit val materializer: ActorMaterializer = ActorMaterializer()
    val httpHost = "0.0.0.0"
    val httpPort = settings.rpcPort

    // trigger the P2P network bind and check that the protocol bound successfully. Terminate the application on failure
    (networkControllerRef ? "Bind").onComplete {
      case Success(_) =>
        log.info(s"${Console.YELLOW}P2P is server bound and in the operational state${Console.RESET}")

      case Failure(ex) =>
        log.error(s"${Console.RED}Unable to bind to the P2P port. Terminating application!${Console.RESET}", ex)
        BifrostApp.shutdown(actorSystem, actorsToStop)
    }

    // trigger the HTTP server bind and check that the bind is successful. Terminate the application on failure
    Http().bindAndHandle(httpService.compositeRoute, httpHost, httpPort).onComplete {
      case Success(serverBinding) =>
        log.info(s"${Console.YELLOW}HTTP server bound to ${serverBinding.localAddress}${Console.RESET}")

      case Failure(ex) =>
        log.error(s"${Console.YELLOW}Failed to bind to ${httpHost}:${httpPort}. Terminating application!${Console.RESET}", ex)
        BifrostApp.shutdown(actorSystem, actorsToStop)
    }

  }
}

object BifrostApp extends Logging {
  private val conf: Config = ConfigFactory.load("application")
  if (conf.getBoolean("kamon.enable")) Kamon.init()

  import com.joefkelley.argyle._

  val argParser: Arg[StartupOpts] = (
    optional[String]("--config", "-c") and
      optionalOneOf[NetworkType](NetworkType.all.map(x => s"--${x.verboseName}" -> x): _*)
    ).to[StartupOpts]

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