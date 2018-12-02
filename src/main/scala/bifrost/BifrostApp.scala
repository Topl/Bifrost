package bifrost

import akka.actor.{Actor, ActorRef, AllDeadLetters, DeadLetter, Props}
import akka.event.Logging
import bifrost.api.http._
import bifrost.blocks.BifrostBlock
import bifrost.forging.{Forger, ForgingSettings}
import bifrost.history.BifrostSyncInfoMessageSpec
import bifrost.network.BifrostNodeViewSynchronizer
import bifrost.scorexMod.GenericApplication
import bifrost.scorexMod.api.http.GenericNodeViewApiRoute
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box.BifrostBox
import io.circe
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.network.message.MessageSpec
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import scorex.core.transaction.state.PrivateKey25519

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

  override val apiRoutes: Seq[ApiRoute] = Seq(
    DebugApiRoute(settings, nodeViewHolderRef),
    WalletApiRoute(settings, nodeViewHolderRef),
    ContractApiRoute(settings, nodeViewHolderRef, networkController),
    AssetApiRoute(settings, nodeViewHolderRef),
    WalletApiRouteRPC(settings, nodeViewHolderRef),
    UtilsApiRoute(settings),
    GenericNodeViewApiRoute[P, TX](settings, nodeViewHolderRef),
    PeersApiRoute(peerManagerRef, networkController, settings)
  )

  override val apiTypes: Seq[Type] = Seq(typeOf[UtilsApiRoute],
                                         typeOf[DebugApiRoute],
                                         typeOf[WalletApiRoute],
                                         typeOf[ContractApiRoute],
                                         typeOf[AssetApiRoute],
                                         typeOf[WalletApiRouteRPC],
                                         typeOf[GenericNodeViewApiRoute[P, TX]],
                                         typeOf[PeersApiRoute])

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

  class DeadLetterMonitor extends Actor {
    def receive = {
      case msg: AllDeadLetters => log.debug(s"${self.path.name} - dead letter encountered: $msg")
    }
  }

  val listener = actorSystem.actorOf(Props(new DeadLetterMonitor))
  actorSystem.eventStream.subscribe(listener, classOf[AllDeadLetters])

  class CheckThreadsRunner extends Thread {

    override def run(): Unit =  while(true) {
      import scala.collection.JavaConverters._
      val dispThreads =
        Thread.getAllStackTraces.keySet.asScala.filter(_.getName startsWith "default-akka.actor.default-dispatcher")

      dispThreads.toVector.map(_.getName).sorted.foreach(log.debug)
      log.debug(s"\nCurrently ${dispThreads.size} threads")

      Thread.sleep(10000)
    }
  }

  val checker = new CheckThreadsRunner
  checker.setDaemon(true)
  checker.start()

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
  val settingsFilename = args.headOption.getOrElse("testnet-valhalla.json")
//val settingsFilename = args.headOption.getOrElse("settings.json")
  new BifrostApp(settingsFilename).run()
}