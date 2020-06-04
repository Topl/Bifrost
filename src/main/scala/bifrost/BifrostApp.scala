package bifrost

import java.lang.management.ManagementFactory
import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import bifrost.api.http.{ApiRoute, UtilsApiRoute, _}
import bifrost.crypto.PrivateKey25519
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.modifier.block.Block
import bifrost.modifier.box.Box
import bifrost.modifier.box.proposition.ProofOfKnowledgeProposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.network.message._
import bifrost.network.peer.PeerManager
import bifrost.network._
import bifrost.nodeView.NodeViewHolder
import bifrost.settings.{BifrostContext, AppSettings}
import bifrost.utils.{Logging, NetworkTimeProvider}
import com.sun.management.HotSpotDiagnosticMXBean
import com.typesafe.config.{Config, ConfigFactory}
import io.circe
import kamon.Kamon

import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe._

class BifrostApp(val settingsFilename: String) extends Logging with Runnable {

  import bifrost.network.NetworkController.ReceivableMessages.ShutdownNetwork

  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = Box
  type TX = Transaction
  type PMOD = Block
  type NVHT = NodeViewHolder

  //settings
  implicit val settings: AppSettings

  private val conf: Config = ConfigFactory.load("application")
  private val ApplicationNameLimit: Int = conf.getInt("app.applicationNameLimit")

//  implicit lazy val settings = new ForgingSettings {
//    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
//  }
  log.debug(s"Starting application with settings \n$settings")

  /* networkController */
  protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BifrostSyncInfoMessageSpec)

//  private lazy val basicSpecs =
//    Seq(
//      GetPeersSpec,
//      PeersSpec,
//      InvSpec,
//      RequestModifierSpec,
//      ModifiersSpec
//    )
//
//  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)
//
//  lazy val upnp = new UPnP(settings)
//
//  protected implicit lazy val actorSystem = ActorSystem(settings.agentName)
//
//
//
//  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
//  val networkControllerRef = actorSystem.actorOf(nProps, "networkController")

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("bifrost.executionContext")

  protected val features: Seq[PeerFeature]
  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  //p2p
  private val upnpGateway: Option[UPnPGateway] = if (settings.network.upnpEnabled) UPnP.getValidGateway(settings.network) else None
  // TODO use available port on gateway instead settings.network.bindAddress.getPort
  upnpGateway.foreach(_.addPort(settings.network.bindAddress.getPort))

  private lazy val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)
    val featureSerializers: PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap
    Seq(
      GetPeersSpec,
      new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects),
      invSpec,
      requestModifierSpec,
      modifiersSpec
    )
  }

  val timeProvider = new NetworkTimeProvider(settings.ntp)

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.network.declaredAddress orElse {
      // TODO use available port on gateway instead settings.bindAddress.getPort
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, settings.network.bindAddress.getPort))
    }
  }

  val context = BifrostContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  val peerManagerRef = actorSystem.actorOf(Props(classOf[PeerManager], settings), "peerManager")

  val networkControllerRef: ActorRef = NetworkControllerRef("networkController", settings.network, peerManagerRef, context)



  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  /* nodeViewHoderRef */
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new NVHT(settings)), "nodeViewHolder")
  /* ---------------- */

  /* forger */
  val forgerRef: ActorRef = ForgerRef("forger", settings.forgingSettings, nodeViewHolderRef))

  val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forgerRef, settings), "localInterface"
  )
  /* -------------- */

  val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[NodeViewSynchronizer],
      networkControllerRef,
      nodeViewHolderRef,
      localInterface,
      BifrostSyncInfoMessageSpec), "nodeViewSynchronizer"
  )

  val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ProgramApiRoute(settings, nodeViewHolderRef, networkControllerRef),
    AssetApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
//    GenericNodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
//    PeersApiRoute(peerManagerRef, networkController, settings),
    NodeViewApiRoute(settings, nodeViewHolderRef)
  )

  val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute],
    typeOf[DebugApiRoute],
    typeOf[WalletApiRoute],
    typeOf[ProgramApiRoute],
    typeOf[AssetApiRoute],
//    typeOf[GenericNodeViewApiRoute[P, TX]],
//    typeOf[PeersApiRoute],
    typeOf[NodeViewApiRoute])

  lazy val combinedRoute = CompositeHttpService(actorSystem, apiTypes, apiRoutes, settings).compositeRoute

  // Am I running on a JDK that supports JVMCI?
  val vm_version = System.getProperty("java.vm.version")
  System.out.printf("java.vm.version = %s%n", vm_version)

  // Is JVMCI enabled?
  val bean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
  val enableJVMCI = bean.getVMOption("EnableJVMCI")
  System.out.println(enableJVMCI)

  // Is the system using the JVMCI compiler for normal compilations?
  val useJVMCICompiler = bean.getVMOption("UseJVMCICompiler")
  System.out.println(useJVMCICompiler)

  // What compiler is selected?
  val compiler = System.getProperty("jvmci.Compiler")
  System.out.printf("jvmci.Compiler = %s%n", compiler)

  def run(): Unit = {
    require(settings.agentName.length <= ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at 0.0.0.0:${settings.rpcPort}")

    implicit val materializer = ActorMaterializer()
    Http().bindAndHandle(combinedRoute, "0.0.0.0", settings.rpcPort)

    /* on unexpected shutdown */
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        stopAll()
      }
    })
  }

  def stopAll(): Unit = synchronized {
    log.info("Stopping network services")
    if (settings.upnpEnabled) upnpGateway.foreach(_.deletePort(settings.network.bindAddress.getPort))
    networkControllerRef ! ShutdownNetwork

    log.info("Stopping actors (incl. block generator)")

    actorSystem.terminate().onComplete { _ =>
      log.info("Exiting from the app...")
      System.exit(0)
    }
  }
}

object BifrostApp extends App {
  private val conf: Config = ConfigFactory.load("application")
  if (conf.getBoolean("kamon.enable")) Kamon.init()
  val settingsFilename = args.headOption.getOrElse("testnet-private.json")
  new BifrostApp(settingsFilename).run()
}