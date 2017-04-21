package examples.bifrost

import akka.actor.{ActorRef, Props}
import examples.bifrost.api.http.{DebugApiRoute, WalletApiRoute}
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.forging.{Forger, ForgingSettings}
import examples.bifrost.history.{BifrostSyncInfo, BifrostSyncInfoMessageSpec}
import examples.bifrost.scorexMod.{GenericApplication, GenericNodeViewSynchronizer}
import examples.bifrost.transaction.BifrostTransaction
import examples.bifrost.transaction.box.BifrostBox
import examples.bifrost.wallet.StableCoinTransferGenerator
import examples.bifrost.wallet.StableCoinTransferGenerator.StartGeneration
import io.circe
import scorex.core.api.http.{ApiRoute, NodeViewApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.network.message.MessageSpec
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519

import scala.concurrent.duration._
import scala.reflect.runtime.universe._

class BifrostApp(val settingsFilename: String) extends GenericApplication {

  override type P = ProofOfKnowledgeProposition[PrivateKey25519]
  override type BX = BifrostBox
  override type TX = BifrostTransaction
  override type PMOD = BifrostBlock
  override type NVHT = BifrostNodeViewHolder

  implicit lazy val settings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }
  log.debug(s"Starting application with settings \n$settings")

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BifrostSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[NVHT], settings))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
    NodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkController, settings)
  )

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute], typeOf[DebugApiRoute], typeOf[WalletApiRoute],
    typeOf[NodeViewApiRoute[P, TX]], typeOf[PeersApiRoute])

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(classOf[GenericNodeViewSynchronizer[P, TX, BifrostSyncInfo, BifrostSyncInfoMessageSpec.type]],
      networkController, nodeViewHolderRef, localInterface, BifrostSyncInfoMessageSpec))

  //touching lazy vals
  forger
  localInterface
  nodeViewSynchronizer

  if (settings.nodeName == "node1") {
    log.info("Starting transactions generation")
    val generator: ActorRef = actorSystem.actorOf(Props(classOf[StableCoinTransferGenerator], nodeViewHolderRef))
    generator ! StartGeneration(FiniteDuration(10, SECONDS))
  }
}

object BifrostApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings.json")
  new BifrostApp(settingsFilename).run()
}