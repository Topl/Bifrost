package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.CurrentView
import bifrost.state.BifrostState
import bifrost.transaction.{AssetRedemption, AssetTransfer}
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

import scala.concurrent.ExecutionContext.Implicits.global
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
            case Right(json) => Try {
              val id = (json \\ "id").head.asString.get
              reqId = id
              require((json \\ "jsonrpc").head.asString.get == "2.0")
              val params = (json \\ "params").head.asArray.get
              require(params.size <= 5, s"size of params is ${params.size}")
              (json \\ "method").head.asString.get match {
                case "redeemAssets" => redeemAssets(view, params.head, id).asJson
                case "transferAssets" => transferAssets(view, params.head, id).asJson
              }
            } match {
              case Success(resp) => BifrostSuccessResponse(resp, reqId)
              case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", false.asJson).asBoolean.get)
            }
          }
        }
      }
    }
  }}

  private def redeemAssets(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String) = {
    val wallet = view.vault

    val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
    val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get

    val tempTx = params.as[AssetRedemption] match {
      case Right(a: AssetRedemption) => a
      case Left(e) => throw new Exception(s"Could not parse AssetRedemption: $e")
    }

    val realSignature = PrivateKey25519Companion.sign(selectedSecret, tempTx.messageToSign)
    val modifiedSignatures = tempTx.signatures.map { case (key, fakeSigs) =>
      (key, fakeSigs.map(_ => realSignature))
    }
    val tx = tempTx.copy(signatures = modifiedSignatures)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetRedemption](tx)
    tx.json
  }

  private def transferAssets(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String) = {
    val wallet = view.vault

    val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
    val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
    val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
    val hub = PublicKey25519Proposition(Base58.decode((params \\ "hub").head.asString.get).get)
    val assetCode: String = (params \\ "assetCode").head.asString.getOrElse("")
    val tx = AssetTransfer.create(wallet, IndexedSeq((recipient, amount)), fee, hub, assetCode).get
    nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], AssetTransfer](tx)
    tx.json
  }
}
