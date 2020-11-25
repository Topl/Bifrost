package co.topl.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.{Http, HttpExt}
import akka.pattern.pipe
import akka.util.Timeout
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.PublicKeyPropositionCurve25519
import co.topl.http.api.ApiEndpoint
import co.topl.http.api.endpoints.TransactionApiEndpoint
import co.topl.modifier.block.Block
import co.topl.modifier.transaction._
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ModificationOutcome, SemanticallySuccessfulModifier}
import co.topl.settings.{AppContext, AppSettings, RPCApiSettings}
import co.topl.utils.Logging
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success


/**
  * Manages the communication between Bifrost and a running wallet.
  * @param settings - the current AppSettings from Bifrost.
  * @param ec - the execution context used for futures.
  */
class WalletConnectionHandler(settings: RPCApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
                             (implicit ec: ExecutionContext, networkPrefix: NetworkPrefix) extends Actor with Logging {

  import WalletConnectionHandler._

  implicit val timeout: Timeout = 10.seconds
  implicit val actorSystem: ActorSystem = context.system

  var remoteWalletActor: Option[ActorRef] = None
  var remoteWalletKeys: Option[Set[PublicKeyPropositionCurve25519]] = None

  val http: HttpExt = Http(context.system)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  override def receive: Receive = {

    case msg: String => msgHandler(msg)

    case GetRemoteWalletRef => sender ! remoteWalletActor

    case SemanticallySuccessfulModifier(block: Block) =>
      remoteWalletActor.map { actor =>
        parseBlockForKeys(block).map(txJson => actor ! s"new block added: $txJson")
      }
  }

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /**
    * Parses a block, looking for the public keys from the remote wallet.
    * @param block - a new block that was just added.
    * @return - returns json of the transactions from the new block if it contains public keys from the remote wallet.
    *         Otherwise, returns None.
    */
  def parseBlockForKeys(block: Block): Option[Json] = remoteWalletKeys map { keys =>
    val txs: Seq[Transaction.TX] = block.transactions.filter {
      case tx: TransferTransaction[_] if keys.toSeq.intersect(tx.to.map(_._1)).nonEmpty => true
      case _ => false
    }

    txs.asJson
  }

  /**
   * Handles requests sent from a remote Gjallarhorn instance and sends them to the appropriate API methods
   * @param apiService the service to target for processing of the request
   * @param req parameters to fulfill the request
   * @param actorRef the actor to respond to
   */
  def processRequest(apiService: ApiEndpoint, req: (String, Vector[Json], String), actorRef: ActorRef): Unit = {
    apiService
      .handlers(req._1, req._2, req._3)
      .transformWith {
        case Success(resp) => Future(resp.noSpaces)
        case _ => Future("Failed to process request")
      }
      .pipeTo(actorRef)
  }

  /**
   * Parse incoming request parameters and target the service with the appropriate handler function
   * @param params function parameters needed to process the requested message type
   * @param walletRef the actor reference of the Gjallarhorn instance
   * @param requestType type of request being sent
   */
  def sendRequestApi(params: String, walletRef: ActorRef, requestType: String): Unit = {
    parse(params) match {
      case Right(tx) =>
        val id = (tx \\ "id").head.asString.get
        val params = (tx \\ "params").head.asArray.get
        require(params.size <= 1, s"size of params is ${params.size}")
        val method = (tx \\ "method").head.asString.get
        requestType match {
          case "asset" =>
        }

        requestType match {
          case "transfer" =>
            processRequest(
              TransactionApiEndpoint(settings, appContext, nodeViewHolderRef),
              (method, params, id),
              walletRef
            )
        }

      case Left(error) => throw new Exception(s"error: $error")
    }
  }

  /**
   * Parse the set of keys registered by the Gjallarhorn actor
   * @param keys a stringified set of PublicKeyPropositions to monitor for changes
   */
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

      remoteWalletKeys =
        Some(keystrings.map(key => PublicKeyPropositionCurve25519(key)))
    }
  }

  /**
   *
   * @param msg
   */
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
      remoteWalletKeys = None
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
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object WalletConnectionHandler {
  val actorName = "walletConnectionHandler"

  case object GetRemoteWalletRef
}


////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object WalletConnectionHandlerRef {
  def props (settings: AppSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
            (implicit ec: ExecutionContext): Props =
    Props(
      new WalletConnectionHandler(settings.rpcApi, appContext, nodeViewHolderRef)
      (ec, appContext.networkType.netPrefix)
    )

  def apply (name: String, settings: AppSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
            (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, appContext, nodeViewHolderRef), name)
}