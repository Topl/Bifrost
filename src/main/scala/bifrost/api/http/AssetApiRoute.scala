package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.exceptions.JsonParsingException
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.CurrentView
import bifrost.state.BifrostState
import bifrost.transaction._
import bifrost.transaction.box.ProfileBox
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.ApiException
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

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
            case Right(json) =>
              val futureResponse: Try[Future[Json]] = Try {
              val id = (json \\ "id").head.asString.get
              reqId = id
              require((json \\ "jsonrpc").head.asString.get == "2.0")
              val params = (json \\ "params").head.asArray.get
              require(params.size <= 5, s"size of params is ${params.size}")

              (json \\ "method").head.asString.get match {
                case "redeemAssets" => redeemAssets(params.head, id)
                case "transferAssets" => transferAssets(params.head, id)
                case "createAssets" => createAssets(params.head, id)
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
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetRedemption](tx)
      tx.json
    }
  }

  private def transferAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault

      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val hub = PublicKey25519Proposition(Base58.decode((params \\ "hub").head.asString.get).get)
      val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")
      val data: String = (params \\ "data").head.asString.getOrElse("")
      val publicKeysToSendFrom: Vector[String] = (params \\ "publicKeyToSendFrom").headOption match {
        case Some(keys: Json) => keys.asArray.get.map(k => k.asString.get)
        case None => Vector()
      }
      val publicKeyToSendChangeTo: String = (params \\ "publicKeyToSendChangeTo").headOption match {
        case Some(key) => key.asString.get
        case None => if (publicKeysToSendFrom.nonEmpty) publicKeysToSendFrom.head else ""
      }
      val tx = AssetTransfer.create(wallet, IndexedSeq((recipient, amount)), fee, hub, assetCode, data, publicKeysToSendFrom, publicKeyToSendChangeTo).get

      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetTransfer](tx)
      tx.json
    }
  }

  private def createAssets(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault

      val hub = PublicKey25519Proposition(Base58.decode((params \\ "hub").head.asString.get).get)
      val to: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "to").head.asString.get).get)
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      val tx = AssetCreation.createAndApply(wallet, IndexedSeq((to, amount)), fee, hub, assetCode, data).get
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetCreation](tx)
      //    println("----------------------")
      //    println("validating transaction")
      //    println(AssetCreation.validate(tx))
      tx.json
    }
  }
}
