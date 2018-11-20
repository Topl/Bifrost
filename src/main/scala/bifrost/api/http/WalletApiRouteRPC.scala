package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.transaction.PolyTransfer
import bifrost.transaction.box.{ArbitBox, PolyBox}
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.parser._
import io.circe.syntax._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

case class WalletApiRouteRPC(override val settings: Settings, nodeViewHolderRef: ActorRef)
                       (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool
  override val route: Route = pathPrefix("wallet") { walletRoute }

  //noinspection ScalaStyle
  def walletRoute: Route = path("") { entity(as[String]) { body =>
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
                  case "transfer" => transfer(params.head, id)
                  case "balances" => balances(params.head, id)
                  case "balancesByKey" => balancesByKey(params.head, id)
                  case "unlockKeyfile" => unlockKeyfile(params.head, id)
                  case "generateKeyfile" => generateKeyfile(params.head, id)
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

  private def transfer(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").head.asString.get
      val tx = PolyTransfer.create(wallet, IndexedSeq((recipient, amount)), fee, data).get
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
      tx.json
    }
  }

  private def balances(params: Json, id: String): Future[Json] = {
      viewAsync().map { view =>
        val wallet = view.vault
        val boxes = wallet.boxes()

        Map("polyBalance" -> boxes.flatMap(_.box match {
            case pb: PolyBox => Some(pb.value)
            case _ => None
          }).sum.toString.asJson,
          "arbitBalance" -> boxes.flatMap(_.box match {
            case ab: ArbitBox => Some(ab.value)
            case _ => None
          }).sum.toString.asJson,
          "publicKeys" -> wallet.publicKeys.flatMap(_ match {
            case pkp: PublicKey25519Proposition => Some(Base58.encode(pkp.pubKeyBytes))
            case _ => None
          }).asJson,
          "boxes" -> boxes.map(_.box.json).asJson
        ).asJson
      }
    }

  private def balancesByKey(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val publicKey: String = (params \\ "publicKey").head.asString.get

      val boxes = wallet.boxesByKey(publicKey)

      Map("polyBalance" -> boxes.flatMap(_.box match {
        case pb: PolyBox => Some(pb.value)
        case _ => None
      }).sum.toString.asJson,
        "arbitBalance" -> boxes.flatMap(_.box match {
          case ab: ArbitBox => Some(ab.value)
          case _ => None
        }).sum.toString.asJson,
        "publicKeys" -> wallet.publicKeys.flatMap(_ match {
          case pkp: PublicKey25519Proposition => Some(Base58.encode(pkp.pubKeyBytes))
          case _ => None
        }).asJson,
        "boxes" -> boxes.map(_.box.json).asJson
      ).asJson
    }
  }

  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val password: String = (params \\ "password").head.asString.get
      val pubKey = wallet.generateNewSecret(password)
      Map(
        "publicKey" -> Base58.encode(pubKey.pubKeyBytes).asJson
      ).asJson
    }
  }

  private def unlockKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val publicKey: String = (params \\ "publicKey").head.asString.get
      val password: String = (params \\ "password").head.asString.get
      wallet.unlockKeyFile(publicKey, password)
      Map(
        publicKey -> "unlocked".asJson
      ).asJson
    }
  }

}
