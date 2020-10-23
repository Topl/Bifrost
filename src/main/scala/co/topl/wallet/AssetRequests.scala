package co.topl.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import co.topl.modifier.transaction.{AssetCreation, AssetTransfer}
import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{CurrentView, NodeViewHolder}
import co.topl.nodeView.state.box.{AssetBox, BoxId}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.utils.Logging
import co.topl.wallet.AssetRequests.AssetRequest
import io.circe.{Decoder, Json}
import io.circe.syntax._
import scorex.util.encode.Base58

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration._

class AssetRequests (nodeViewHolderRef: ActorRef)
                    ( implicit ec: ExecutionContext ) extends Actor with Logging {

  implicit val timeout: Timeout = 10.seconds

  def requestFromWallet(method: String, params: Json): Future[String] = {
    method match {
      case "transferAssetsPrototype" => transferAssetsPrototype(params)
      case "transferTargetAssetsPrototype" => transferTargetAssetsPrototype(params)
      case "createAssetsPrototype" => createAssetsPrototype(params)
    }
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


  /**
    * @param params input parameter as specified above
    * @return
    */
  private def transferAssetsPrototype(implicit params: Json): Future[String] = {
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]].map{ view =>
      // parse arguments from the request
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition((params \\ "recipient").head.asString.get)
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map { key =>
          PublicKey25519Proposition(key.asString.get)
        }
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).get
      val issuer = PublicKey25519Proposition((params \\ "issuer").head.asString.get)
      val assetCode: String = (params \\ "assetCode").head.asString.get
      val data: String = parseOptional("data", "")

      // check that the transaction can be constructed
      if (!view.state.hasTBR)
        throw new Exception("TokenBoxRegistry not defined for node")

      //YT NOTE - if nodeKeys not defined in settings file then node watches for all keys in a state update
      if (view.state.nodeKeys.isDefined && !sender.forall(view.state.nodeKeys.contains(_)))
        throw new Exception("Node not set to watch for specified public key")


      // construct the transaction
      val tx = AssetTransfer
        .createRaw(view.state, IndexedSeq((recipient, amount)), sender, issuer, assetCode, fee, data)
        .get

      // validate and update nodeView with new TX
      AssetTransfer.validatePrototype(tx) match {
        case Success(_) =>
          Map(
            "formattedTx" -> tx.json,
            "messageToSign" -> Base58.encode(tx.messageToSign).asJson
          ).asJson.noSpaces
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  /**
    * @param params input parameters as specified above
    * @return
    */
  private def transferTargetAssetsPrototype(implicit params: Json): Future[String] = {
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]].map { view =>
      // parse required arguments from the request
      val sender: IndexedSeq[PublicKey25519Proposition] =
        (params \\ "sender").head.asArray.get.map { key =>
          PublicKey25519Proposition(key.asString.get)
        }
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition((params \\ "recipient").head.asString.get)
      val assetId: BoxId = BoxId((params \\ "assetId").head.asString.get)
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = parseOptional("data", "")

      val asset = view.state
        .getBox(assetId) match {
        case Some(b: AssetBox) => b
        case _ => throw new Error(s"Failed to find specified bow with id: $assetId")
      }

      val tx =
        AssetTransfer
          .createRaw(view.state, IndexedSeq((recipient, amount)), sender, asset.issuer, asset.assetCode, fee, data)
          .get

      AssetTransfer.validatePrototype(tx) match {
        case Success(_) =>
          Map(
            "formattedTx" -> tx.json,
            "messageToSign" -> Base58.encode(tx.messageToSign).asJson
          ).asJson.noSpaces
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }


  /**
    * @param params input parameters as specified above
    * @return
    */
  private def createAssetsPrototype(implicit params: Json): Future[String] = {
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]].map { view =>
      val issuer = PublicKey25519Proposition((params \\ "issuer").head.asString.get)
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition((params \\ "recipient").head.asString.get)
      println("recipient: " + (params \\ "recipient").head.asString.get)
      println("pub key: " + recipient)
      println("pub key bytes: " + recipient.pubKeyBytes + " of length " + recipient.pubKeyBytes.length)
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val assetCode: String = (params \\ "assetCode").head.asString.get
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).get
      val data: String = parseOptional("data", "")

      val tx =
        AssetCreation
          .createRaw(IndexedSeq((recipient, amount)), fee, issuer, assetCode, data)
          .get
      AssetCreation.validatePrototype(tx) match {
        case Success(_) =>
          Map(
            "formattedTx" -> tx.json,
            "messageToSign" -> Base58.encode(tx.messageToSign).asJson
          ).asJson.noSpaces
        case Failure(e) =>
          throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  override def receive: Receive =  {
    case AssetRequest(tx: Json) => {
      val method: String = (tx \\ "method").head.asString.get
      val future: Future[String] = requestFromWallet(method, (tx \\ "params").head)
      future.pipeTo(sender())
    }
  }

}

object AssetRequests {
  val actorName = "assetRequests"

  case class AssetRequest(tx: Json)

}
