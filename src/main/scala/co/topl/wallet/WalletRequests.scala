package co.topl.wallet

import akka.actor.{Actor, ActorRef}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import co.topl.modifier.transaction.{ArbitTransfer, AssetCreation, AssetTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.CurrentView
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.TokenBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.utils.Logging
import co.topl.wallet.WalletRequests.WalletRequest
import io.circe.{Decoder, Json}
import io.circe.syntax._
import scorex.util.encode.Base58

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class WalletRequests (nodeViewHolderRef: ActorRef)
                     (implicit  ec: ExecutionContext) extends Actor with Logging {

  implicit val timeout: Timeout = 10.seconds

  type CV = CurrentView[History, State, MemPool]

  def requestFromWallet ( method: String, params: Json, id: String): Future[String] =
    method match {
      case "transferArbitsPrototype" => transferArbitsPrototype(params)
      case "transferPolysPrototype"  => transferPolysPrototype(params)
      case "balances"                => balances(params)
      //      case "signTx"                  => signTx(params)
      case "broadcastTx"             => broadcastTx(params, id)
    }

  /**
    * Helper function to parse optional parameters from the request
    * @param key optional key to be looked for
    * @param default default return value
    * @tparam A type of the value expected to be retrieved
    * @return the provided value or the default
    */
  def parseOptional[A](key: String, default: A)(implicit params: Json, decode: Decoder[A]): A = {
    params.hcursor.downField(key).as[A] match {
      case Right(value) => value
      case Left(_)      => default
    }
  }

  def checkPublicKey (keys: Seq[PublicKey25519Proposition], view: CV): Unit = {
    if ( !view.state.hasTBR )
      throw new Exception("TokenBoxRegistry not defined for node")

    //YT NOTE - if nodeKeys not defined in settings file then node watches for all keys in a state update
    if ( view.state.nodeKeys.isDefined && !keys.forall(key => view.state.nodeKeys.contains(key)) )
      throw new Exception("Node not set to watch for specified public key")
  }

  /**
  * @param params input parameters as specified above
  * //@param id     request identifier
    * @return
  */
  private def transferArbitsPrototype ( implicit params: Json): Future[String] = {
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CV].map{ view =>
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition((params \\ "recipient").head.asString.get)
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(key.asString.get))
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).get
      val data: String = parseOptional("data", "")

      checkPublicKey(sender, view)

      val tx = ArbitTransfer
        .createPrototype(view.state, IndexedSeq((recipient, amount)), sender, fee, data)
        .get

      // Update nodeView with new TX
      ArbitTransfer.validatePrototype(tx) match {
        case Success(_) =>
          tx.json.noSpaces
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**
    * @param params input parameters as specified above
    * //@param id     request identifier
    * @return
    */
  private def transferPolysPrototype ( implicit params: Json): Future[String] = {
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CV].map{ view =>
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition((params \\ "recipient").head.asString.get)
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(key.asString.get))
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).get
      val data: String = parseOptional("data", "")

      checkPublicKey(sender, view)

      val tx = PolyTransfer
        .createPrototype(
          view.state,
          IndexedSeq((recipient, amount)),
          sender,
          fee,
          data
        )
        .get

      // Update nodeView with new TX
      PolyTransfer.validatePrototype(tx) match {
        case Success(_) =>
          tx.json.noSpaces
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**
    * @param params input parameters as specified above
    * //@param id     request identifier
    * @return
    */
  private def balances ( params: Json): Future[String] = {
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CV].map{ view =>
      // parse the required arguments from the request
      val publicKeys = (params \\ "publicKeys").head.asArray.get.map(k => PublicKey25519Proposition(k.asString.get))

      checkPublicKey(publicKeys, view)

      val boxes: Map[PublicKey25519Proposition, Map[String, Seq[TokenBox]]] =
        publicKeys
          .map(k => {
            val orderedBoxes = view.state.getTokenBoxes(k) match {
              case Some(boxes) =>
                boxes.groupBy[String](b => b.typeOfBox)
              case _           => Map[String, Seq[TokenBox]]()
            }
            k -> orderedBoxes
          }).toMap

      val balances: Map[PublicKey25519Proposition, Map[String, Long]] =
        boxes.map {
          case (prop, assets) => prop -> assets.map {
            case (boxType, boxes) => (boxType, boxes.map(_.value).sum)
          }
        }

      boxes.map {
        case (prop, boxes) =>
          prop.address -> Map(
            "Balances" -> Map(
              "Polys" -> balances(prop).getOrElse("Poly", 0L),
              "Arbits" -> balances(prop).getOrElse("Arbit", 0L),
              "Assets" -> balances(prop).getOrElse("Asset", 0L)
            ).asJson,
            "Boxes" -> boxes.map(b => b._1 -> b._2.map(_.json).asJson).asJson
          )
      }.asJson.noSpaces

    }
  }

  /**
    * @param params input parameters as specified above
    * //@param id     request identifier
    * @return
    */
  private def broadcastTx (params: Json, id: String): Future[String] = {
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CV].map{ view =>
      val tx = (params \\ "tx").head
      val txType = (tx \\ "txType").head.asString.get
      val txInstance: Transaction = txType match {
        case "AssetCreation" =>
          tx.as[AssetCreation] match {
              case Right(asset) => asset
              case Left(f) => throw f
            }
        case "AssetTransfer" => tx.as[AssetTransfer].right.get
        case _               =>
          throw new Exception(s"Could not find valid transaction type $txType")
      }

      State.syntacticValidity(txInstance)
      nodeViewHolderRef ! LocallyGeneratedTransaction[Transaction](txInstance)
      txInstance.json.noSpaces
    }
  }

  override def receive: Receive = {
    case WalletRequest (params: Json) =>
      val method: String = (params \\ "method").head.asString.get
      val future: Future[String] = requestFromWallet(method, (params \\ "params").head, (params \\ "id").head.asString.get)
      future.pipeTo(sender())
  }

}

object WalletRequests {
  val actorName = "walletRequests"

  case class WalletRequest(params: Json)
}
