package bifrost.api.http

import java.time.Instant

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.exceptions.JsonParsingException
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import bifrost.LocalInterface.LocallyGeneratedTransaction
import bifrost.settings.Settings
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction.{AssetCreation, AssetRedemption, AssetTransfer}
import bifrost.transaction.box.AssetBox
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
  * Created by cykoz on 7/3/2017.
  */
case class AssetApiRoute (override val settings: Settings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool
  override val route: Route = pathPrefix("asset") { assetRoute }

  //noinspection ScalaStyle
  def assetRoute: Route = path("") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map { view =>
          var reqId = ""
          parse(body) match {
            case Left(failure) => ApiException(failure.getCause)
            case Right(request) =>
              val futureResponse: Try[Future[Json]] = Try {
                val id = (request \\ "id").head.asString.get
                reqId = id
                require((request \\ "jsonrpc").head.asString.get == "2.0")
                val params = (request \\ "params").head.asArray.get
                require(params.size <= 5, s"size of params is ${params.size}")

                (request \\ "method").head.asString.get match {
                  case "redeemAssets" => redeemAssets(params.head, id)
                  case "transferAssets" => transferAssets(params.head, id)
                  case "transferAssetsPrototype" => transferAssetsPrototype(params.head, id)
                  case "transferTargetAssets" => transferTargetAssets(params.head, id)
                  case "createAssets" => createAssets(params.head, id)
                  case "createAssetsPrototype" => createAssetsPrototype(params.head, id)
                }
              }
              futureResponse map {
                response => Await.result(response, timeout.duration)
              }
              match {
                case Success(resp) => BifrostSuccessResponse(resp, reqId)
                case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", false.asJson).asBoolean.get)
              }
          }
        }
      }
    }
  }}

  private def redeemAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val tempTx = params.as[AssetRedemption] match {
        case Right(a: AssetRedemption) => a
        case Left(e) => throw new JsonParsingException(s"Could not parse AssetRedemption: $e")
      }
      val realSignature = PrivateKey25519Companion.sign(selectedSecret, tempTx.messageToSign)
      val modifiedSignatures = tempTx.signatures.map { case (key, fakeSigs) =>
        (key, fakeSigs.map(_ => realSignature))
      }
      val tx = tempTx.copy(signatures = modifiedSignatures)
      AssetRedemption.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetRedemption](tx)
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }


  private def transferAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val sender: IndexedSeq[PublicKey25519Proposition] = (params \\ "sender").head.asArray.get.map(key =>
        PublicKey25519Proposition(Base58.decode(key.asString.get).get))
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val issuer = PublicKey25519Proposition(Base58.decode((params \\ "issuer").head.asString.get).get)
      val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      if(view.state.tbr == null) throw new Exception("TokenBoxRegistry not defined for node")
      if(view.state.nodeKeys != null)
        sender.foreach(key => if(!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes)))
          throw new Exception("Node not set to watch for specified public key"))
      val tx = AssetTransfer.create(view.state.tbr, wallet, IndexedSeq((recipient, amount)), sender, fee, issuer, assetCode, data).get
      AssetTransfer.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetTransfer](tx)
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  private def transferAssetsPrototype(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val sender: IndexedSeq[PublicKey25519Proposition] = (params \\ "sender").head.asArray.get.map(key =>
        PublicKey25519Proposition(Base58.decode(key.asString.get).get))
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val issuer = PublicKey25519Proposition(Base58.decode((params \\ "issuer").head.asString.get).get)
      val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }

      if(view.state.tbr == null) throw new Exception("TokenBoxRegistry not defined for node")
      if(view.state.nodeKeys != null)
        sender.foreach(key => if(!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes)))
          throw new Exception("Node not set to watch for specified public key"))
      val tx = AssetTransfer.createPrototype(view.state.tbr, IndexedSeq((recipient, amount)), sender, issuer, assetCode, fee, data).get
      // Update nodeView with new TX
      AssetTransfer.validatePrototype(tx) match {
        case Success(_) =>
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  private def transferTargetAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val assetId: String = (params \\ "assetId").head.asString.getOrElse("")
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }

      val asset = view.state.closedBox(Base58.decode(assetId).get).get.asInstanceOf[AssetBox]
      val selectedSecret: PrivateKey25519 = wallet.secretByPublicImage(asset.proposition).get
      val timestamp = Instant.now.toEpochMilli
      val from: IndexedSeq[(PrivateKey25519, Nonce)] = IndexedSeq((selectedSecret, asset.nonce))
      val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq((recipient, amount))

      val tx = AssetTransfer.apply(from, to, asset.proposition, asset.assetCode, fee, timestamp, data)
      AssetTransfer.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetTransfer](tx)
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  private def createAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val issuer = PublicKey25519Proposition(Base58.decode((params \\ "issuer").head.asString.get).get)
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      val tx = AssetCreation.createAndApply(wallet, IndexedSeq((recipient, amount)), fee, issuer, assetCode, data).get

      AssetCreation.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetCreation](tx)
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  private def createAssetsPrototype(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val issuer = PublicKey25519Proposition(Base58.decode((params \\ "issuer").head.asString.get).get)
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      val tx = AssetCreation.createPrototype(IndexedSeq((recipient, amount)), fee, issuer, assetCode, data).get

      AssetCreation.validatePrototype(tx) match {
        case Success(_) =>
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }
}
