package co.topl.wallet

import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, HttpResponse, MediaTypes, StatusCodes}
import akka.pattern.{ask, pipe}
import akka.util.{ByteString, Timeout}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction._
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ModificationOutcome, SemanticallySuccessfulModifier}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.AppSettings
import co.topl.utils.Logging
import co.topl.wallet.AssetRequests.AssetRequest
import co.topl.wallet.WalletRequests.WalletRequest
import io.circe.{Json, parser}
import io.circe.parser.parse
import io.circe.syntax._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success
import scala.concurrent.duration._


/**
  * Manages the communication between Bifrost and a running wallet.
  * @param settings - the current AppSettings from Bifrost.
  * @param ec - the execution context used for futures.
  */
class WalletConnectionHandler (settings: AppSettings)
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

  def httpPost(jsonRequest: ByteString, path: String): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = path,
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  def requestResponseByteString(request: HttpRequest): Future[ByteString] = {
    val response = http.singleRequest(request)
    response.flatMap {
      case _@HttpResponse(StatusCodes.OK, _, entity, _) =>
        entity.dataBytes.runFold(ByteString.empty) { case (acc, b) => acc ++ b }
      case _ => sys.error("something wrong")
    }
  }

  def byteStringToJSON(data: Future[ByteString]): Json = {
    val parsedData: Future[Json] = data.map { x =>
      parser.parse(x.utf8String) match {
        case Right(parsed) => parsed
        case Left(e) => throw e.getCause
      }
    }
    Await.result(parsedData, 20 seconds)
  }

  /**
    * Parses a block, looking for the public keys from the remote wallet.
    * @param block - a new block that was just added.
    * @return - returns json of the transactions from the new block if it contains public keys from the remote wallet.
    *         Otherwise, returns None.
    */
  def parseBlockForKeys(block: Block): Option[Json] = {
    var txs: Seq[Transaction] = Seq.empty
    block.transactions.foreach(tx =>
      tx match {
        case tx: CodeCreation => if (remoteWalletKeys.contains(tx.to)) txs :+= tx
        case tx: ProgramTransfer => if (remoteWalletKeys.contains(tx.to)) txs :+= tx
        case tx: PolyTransfer => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: ArbitTransfer => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: AssetTransfer => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: AssetCreation => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
        case tx: Coinbase => if (remoteWalletKeys.toSeq.intersect(tx.to.map(_._1)).nonEmpty) txs :+= tx
      }
    )
    if (txs.nonEmpty) Some(txs.asJson)
    else None
  }

  def sendRequestApi(params: String, walletRef: ActorRef, requestType: String): Unit = {
    parse(params) match {
      case Right(tx) =>
        requestType match {
          case "asset" =>
            val sendTx = httpPost(ByteString(tx.toString()), "/asset/")
            val data = requestResponseByteString(sendTx)
            walletRef ! byteStringToJSON(data)
          case "wallet" =>
            val sendTx = httpPost(ByteString(tx.toString()), "/wallet/")
            val data = requestResponseByteString(sendTx)
            walletRef ! byteStringToJSON(data)
        }
      case Left(error) => throw new Exception(s"error: $error")
    }
  }

  def sendRequest(params: String, walletRef: ActorRef, requestType: String): Unit = {
    parse(params) match {
      case Right(tx) =>
        requestType match {
          case "asset" =>
            context.actorSelection("../" + AssetRequests.actorName).resolveOne().onComplete {
              case Success(request: ActorRef) =>
                    val futureResponse = request ? AssetRequest(tx)
                    futureResponse.pipeTo(walletRef)
              case _ =>
                log.warn("No ledger actor found. Can not update view.")
            }
          case "wallet" =>
            context.actorSelection("../" + WalletRequests.actorName).resolveOne().onComplete {
              case Success(request: ActorRef) =>
                val futureResponse = request ? WalletRequest(tx)
                futureResponse.pipeTo(walletRef)
              case _ =>
                log.warn("No ledger actor found. Can not update view.")
            }
        }
      case Left(error)  => throw new Exception (s"error: $error")
    }
  }

  def parseKeys (keys: String): Unit = {
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
      //sendRequest(txString, walletActorRef, "asset")
    }

    if (msg.contains("wallet request:")) {
      val params: String = msg.substring("wallet request: ".length)
      println("Wallet connection handler received wallet request: " + params)
      val walletActorRef: ActorRef = sender()
      sendRequestApi(params, walletActorRef, "wallet")
      //sendRequest(params, walletActorRef, "wallet")
    }
  }

  override def receive: Receive = {

    case msg: String => msgHandler(msg)

    case GetRemoteWalletRef => sender ! remoteWalletActor

    case SemanticallySuccessfulModifier(block: Block) =>
      parseBlockForKeys(block) match {
        case Some(blockJson) =>
            remoteWalletActor match {
              case Some(actor) => actor ! s"new block added: $blockJson"
              case None => System.out.println("no wallet running")
            }
        case None => System.out.println("No keys in new block")
      }

  }
}

object WalletConnectionHandler {
  case object GetRemoteWalletRef
  //case class NewBlockAdded(block: PersistentNodeViewModifier)
}
