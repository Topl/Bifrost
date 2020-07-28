package bifrost

import java.lang.management.ManagementFactory

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
import bifrost.utils.Logging
import com.sun.management.HotSpotDiagnosticMXBean
import com.typesafe.config.{Config, ConfigFactory}
import io.circe
import kamon.Kamon

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe._

class BifrostApp(val settingsFilename: String) extends Logging with Runnable {

  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = Box
  type TX = Transaction
  type PMOD = Block
  type NVHT = NodeViewHolder

  private val conf: Config = ConfigFactory.load("application")
  private val ApplicationNameLimit: Int = conf.getInt("app.applicationNameLimit")

  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  /* networkController */
  protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] =
    Seq(BifrostSyncInfoMessageSpec)

  private lazy val basicSpecs =
    Seq(
      GetPeersSpec,
      PeersSpec,
      InvSpec,
      RequestModifierSpec,
      ModifiersSpec
    )

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  lazy val upnp = new UPnP(settings)

  protected implicit lazy val actorSystem = ActorSystem(settings.agentName)

  val peerManagerRef = actorSystem.actorOf(Props(classOf[PeerManager], settings), "peerManager")

  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
  val networkController = actorSystem.actorOf(nProps, "networkController")
  /* ----------------- */

  /* nodeViewHoderRef */
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new NVHT(settings)), "nodeViewHolder")
  /* ---------------- */

  /* localInterface */
  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef), "forger")

  val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings), "localInterface"
  )
  /* -------------- */

  val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[NodeViewSynchronizer],
      networkController,
      nodeViewHolderRef,
      localInterface,
      BifrostSyncInfoMessageSpec), "nodeViewSynchronizer"
  )

  val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ProgramApiRoute(settings, nodeViewHolderRef, networkController),
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
    if (settings.upnpEnabled) upnp.deletePort(settings.port)
    networkController ! NetworkController.ShutdownNetwork

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