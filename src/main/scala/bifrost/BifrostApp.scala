package bifrost

/*

* @startuml

* car --|> wheel

* @enduml

*/

import akka.actor.{Actor, ActorRef, AllDeadLetters, DeadLetter, Props}
import akka.event.Logging
import bifrost.api.http._
import bifrost.blocks.BifrostBlock
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.history.BifrostSyncInfoMessageSpec
import bifrost.network.BifrostNodeViewSynchronizer
import bifrost.scorexMod.GenericApplication
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box.BifrostBox
import io.circe
import bifrost.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import bifrost.network.message.MessageSpec
import bifrost.transaction.box.proposition.ProofOfKnowledgeProposition
import bifrost.transaction.state.PrivateKey25519

import org.graalvm.polyglot.Context
import java.lang.management.ManagementFactory
import com.sun.management.HotSpotDiagnosticMXBean
import com.sun.management.VMOption

import scala.reflect.runtime.universe._

class BifrostApp(val settingsFilename: String) extends GenericApplication with Runnable {
  // use for debug only
  //  val path: Path = Path ("/tmp")
  //  Try(path.deleteRecursively())

  override type P = ProofOfKnowledgeProposition[PrivateKey25519]
  override type BX = BifrostBox
  override type TX = BifrostTransaction
  override type PMOD = BifrostBlock
  override type NVHT = BifrostNodeViewHolder

  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] =
    Seq(BifrostSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new NVHT(settings)))

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  override val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings)
  )

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostNodeViewSynchronizer],
      networkController,
      nodeViewHolderRef,
      localInterface,
      BifrostSyncInfoMessageSpec)
  )

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRouteHttp(settings, nodeViewHolderRef),
    ContractApiRoute(settings, nodeViewHolderRef, networkController),
    AssetApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
    NodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkController, settings)
  )

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute],
                                         typeOf[DebugApiRoute],
                                         typeOf[WalletApiRouteHttp],
                                         typeOf[ContractApiRoute],
                                         typeOf[AssetApiRoute],
                                         typeOf[WalletApiRoute],
                                         typeOf[NodeViewApiRoute[P, TX]],
                                         typeOf[PeersApiRoute])



  // Am I running on a JDK that supports JVMCI?
         val vm_version = System.getProperty("java.vm.version")
         System.out.printf("java.vm.version = %s%n", vm_version)

         val bean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
  // Is JVMCI enabled?
          val enableJVMCI = bean.getVMOption("EnableJVMCI")
          System.out.println(enableJVMCI)

  // Is the system using the JVMCI compiler for normal compilations?
          val useJVMCICompiler = bean.getVMOption("UseJVMCICompiler")
          System.out.println(useJVMCICompiler)

  // What compiler is selected?
         val compiler = System.getProperty("jvmci.Compiler")
         System.out.printf("jvmci.Compiler = %s%n", compiler)

  //touching lazy vals
  forger
  localInterface
  nodeViewSynchronizer

  /*val scheduler = actorSystem.scheduler
  val task = new Runnable {
    def run(): Unit = {
      networkController ! Message(ProducerNotifySpec, Left(
        ProducerProposal(
          ByteString.copyFrom("testProducer".getBytes),
          ProposalDetails(assetCode = "assetCode"),
          ByteString.copyFrom("signature".getBytes),
          Instant.now.toEpochMilli
        ).toByteArray
      ), Some(null))
    }
  }
  implicit val executor = actorSystem.dispatcher

  scheduler.schedule(initialDelay = Duration(10000, TimeUnit.MILLISECONDS), interval = Duration(7000, TimeUnit.MILLISECONDS), task)*/


  //  if (settings.nodeName == "node1") {
  //    log.info("Starting transactions generation")
  //    val generator: ActorRef = actorSystem.actorOf(Props(classOf[PolyTransferGenerator], nodeViewHolderRef))
  //    generator ! StartGeneration(FiniteDuration(5, SECONDS))
  //  }
}

object BifrostApp extends App {
  val settingsFilename = args.headOption.getOrElse("testnet-private.json")
  new BifrostApp(settingsFilename).run()
}