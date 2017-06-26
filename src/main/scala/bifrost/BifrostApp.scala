package bifrost

import akka.actor.{ActorRef, Props}
import bifrost.api.http._
import bifrost.blocks.BifrostBlock
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.history.{BifrostSyncInfo, BifrostSyncInfoMessageSpec}
import bifrost.scorexMod.{GenericApplication, GenericNodeViewSynchronizer}
import bifrost.scorexMod.api.http.GenericNodeViewApiRoute
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box.BifrostBox
import bifrost.wallet.PolyTransferGenerator
import bifrost.wallet.PolyTransferGenerator.StartGeneration
import io.circe
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.network.message.MessageSpec
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519

import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.reflect.runtime.universe._
import scala.util.Try

class BifrostApp(val settingsFilename: String) extends GenericApplication {
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

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BifrostSyncInfoMessageSpec)

  override val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[NVHT], settings))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ContractApiRoute(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
    GenericNodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkController, settings)
  )

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute], typeOf[DebugApiRoute], typeOf[WalletApiRoute],
    typeOf[ContractApiRoute], typeOf[GenericNodeViewApiRoute[P, TX]], typeOf[PeersApiRoute])

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  override val localInterface: ActorRef = actorSystem.actorOf(Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings))

  override val nodeViewSynchronizer: ActorRef =
    actorSystem.actorOf(Props(classOf[GenericNodeViewSynchronizer[P, TX, BifrostSyncInfo, BifrostSyncInfoMessageSpec.type]],
      networkController, nodeViewHolderRef, localInterface, BifrostSyncInfoMessageSpec))

  //touching lazy vals
  forger
  localInterface
  nodeViewSynchronizer

//  if (settings.nodeName == "node1") {
//    log.info("Starting transactions generation")
//    val generator: ActorRef = actorSystem.actorOf(Props(classOf[PolyTransferGenerator], nodeViewHolderRef))
//    generator ! StartGeneration(FiniteDuration(5, SECONDS))
//  }
}

object BifrostApp extends App {
  val settingsFilename = args.headOption.getOrElse("settings2.json")
  new BifrostApp(settingsFilename).run()
}