package co.topl.wallet

import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.pattern.pipe
import akka.util.Timeout
import co.topl.http.api.routes.{AssetApiRoute, WalletApiRoute}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction._
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ModificationOutcome, SemanticallySuccessfulModifier}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.AppSettings
import co.topl.utils.Logging
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration._


/**
  * Manages the communication between Bifrost and a running wallet.
  * @param settings - the current AppSettings from Bifrost.
  * @param ec - the execution context used for futures.
  */
class WalletConnectionHandler (settings: AppSettings, nodeViewHolderRef: ActorRef)
                              (implicit ec: ExecutionContext) extends Actor with Logging {
  import WalletConnectionHandler._


  implicit val timeout: Timeout = 10.seconds
  implicit val actorAystem: ActorSystem = context.system

  var remoteWalletActor: Option[ActorRef] = None
  var remoteWalletKeys: Set[PublicKey25519Proposition] = Set.empty

  val http: HttpExt = Http(context.system)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
  }

  /**
    * Parses a block, looking for the public keys from the remote wallet.
    * @param block - a new block that was just added.
    * @return - returns json of the transactions from the new block if it contains public keys from the remote wallet.
    *         Otherwise, returns None.
    */
  def parseBlockForKeys(block: Block): Option[Json] = {
    if (remoteWalletKeys.nonEmpty) {
      var txs: Seq[Transaction] = Seq.empty
      block.transactions.foreach {
        case tx: CodeCreation => if (remoteWalletKeys.contains(tx.to)) txs :+= tx
        case tx: ProgramTransfer => if (remoteWalletKeys.contains(tx.to)) txs :+= tx
        case tx: PolyTransfer => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: ArbitTransfer => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: AssetTransfer => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: AssetCreation => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: Coinbase => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
      }
      if (txs.nonEmpty) Some(txs.asJson)
      else None
    }else None
  }

  def sendRequestApi(params: String, walletRef: ActorRef, requestType: String): Unit = {
    parse(params) match {
      case Right(tx) =>
        val id = (tx \\ "id").head.asString.get
        val params = (tx \\ "params").head.asArray.get
        require(params.size <= 1, s"size of params is ${params.size}")
        val method = (tx \\ "method").head.asString.get
        requestType match {
          case "asset" =>
            val assetRoute = AssetApiRoute(settings.restApi, nodeViewHolderRef)
            val futureResponse: Future[Json] = assetRoute.handlers(method, params, id)
            val stringResponse: Future[String] = futureResponse.transformWith {
              case Success(resp) => Future(resp.noSpaces)
              case Failure(ex) => Future("Did not receive a response from asset route.")
            }
            stringResponse.pipeTo(walletRef)
          case "wallet" =>
            val walletRoute = WalletApiRoute(settings.restApi, nodeViewHolderRef)
            val futureResponse: Future[Json] = walletRoute.handlers(method, params, id)
            val stringResponse: Future[String] = futureResponse.transformWith {
              case Success(resp) => Future(resp.noSpaces)
              case Failure(ex) => Future("Did not receive a response from wallet route.")
            }
            stringResponse.pipeTo(walletRef)
        }
      case Left(error) => throw new Exception(s"error: $error")
    }
  }

  def parseKeys (keys: String): Unit = {
    if (keys == "Set()") {
      println("Remote wallet has no keys!")
    }else{
      val keysArr: Array[String] = keys.split(",")
      val keystrings = keysArr.map( key =>
        if (keysArr.indexOf(key) == 0)
          key.substring("Set(".length)
        else if (keysArr.indexOf(key) == keysArr.length-1)
          key.substring(1, key.length-1)
        else key.substring(1)
      ).toSet
      remoteWalletKeys = keystrings.map(key => PublicKey25519Proposition(key))
    }
  }

  def msgHandler(msg: String): Unit = {
    if (msg.contains("Remote wallet actor initialized")) {
      parseKeys(msg.substring("Remote wallet actor initialized. My public keys are: ".length))
      remoteWalletActor = Some(sender())
      remoteWalletActor match {
        case Some(actor) => actor ! s"received new wallet from: ${sender()}"
        case None => println ("no wallets!")
      }
    }
    if (msg == "Remote wallet actor stopped") {
      remoteWalletActor = None
      remoteWalletKeys = Set.empty
      sender ! s"The remote wallet ${sender()} has been removed from the WalletConnectionHandler in Bifrost"
    }

    if (msg.contains("asset transaction:")) {
      val txString: String = msg.substring("asset transaction: ".length)
      println("Wallet Connection handler received asset transaction: " + txString)
      val walletActorRef: ActorRef = sender()
      sendRequestApi(txString, walletActorRef, "asset")
    }

    if (msg.contains("wallet request:")) {
      val params: String = msg.substring("wallet request: ".length)
      println("Wallet connection handler received wallet request: " + params)
      val walletActorRef: ActorRef = sender()
      sendRequestApi(params, walletActorRef, "wallet")
    }
  }

  override def receive: Receive = {

    case msg: String => msgHandler(msg)

    case GetRemoteWalletRef => sender ! remoteWalletActor

    case SemanticallySuccessfulModifier(block: Block) =>
      remoteWalletActor match {
        case Some(actor) =>
          parseBlockForKeys(block) match {
            case Some(blockJson) => actor ! s"new block added: $blockJson"
            case None => System.out.println("no keys in new block")
          }
        case None => System.out.println("No wallet running")
      }

  }
}

object WalletConnectionHandler {
  case object GetRemoteWalletRef
  //case class NewBlockAdded(block: PersistentNodeViewModifier)
}
