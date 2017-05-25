package bifrost.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.transaction.PolyTransfer
import bifrost.transaction.box.{ArbitBox, PolyBox}
import io.circe.parser._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


@Path("/wallet")
@Api(value = "/wallet", produces = "application/json")
case class WalletApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  //TODO move to settings?
  val DefaultFee = 100

  override val route = pathPrefix("wallet") {
    balances ~ transfer ~ sign
  }

  @Path("/transfer")
  @ApiOperation(value = "Transfer",
    notes = "Transfer coins from one output to another",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      dataType = "String",
      paramType = "body"
    )
  ))
  def transfer: Route = path("transfer") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(json) => Try {
                val wallet = view.vault
                val amount: Long = (json \\ "amount").head.asNumber.get.toLong.get
                val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((json \\ "recipient").head.asString.get).get)
                val fee: Long = (json \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(DefaultFee)
                val tx = PolyTransfer.create(wallet, recipient, amount, fee).get
                nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
                tx.json
              } match {
                case Success(resp) => SuccessApiResponse(resp)
                case Failure(e) => ApiException(e)
              }
            }
          }
        }
      }
    }
  }


  @Path("/balances")
  @ApiOperation(value = "Balances", notes = "Return info about local wallet", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def balances: Route = path("balances") {
    getJsonRoute {
      viewAsync().map { view =>
        val wallet = view.vault
        val boxes = wallet.boxes()

        SuccessApiResponse(Map(
          "polyBalance" -> boxes.flatMap(_.box match {
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
        ).asJson)
      }
    }
  }

  @Path("/sign/{messageToSign}")
  @ApiOperation(value = "Sign a message", notes = "Sign a message", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "messageToSign",
      value = "messageToSign in String format",
      required = true,
      dataType = "string",
      paramType = "path"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def sign: Route = path("sign" / Segment) { messageToSign =>
    withAuth {
      getJsonRoute {
        viewAsync().map { view =>
          val wallet = view.vault
          val secrets = wallet.secrets
          val privKey = secrets.toSeq(0)

          SuccessApiResponse(Map(
            "signature" -> Base58.encode(PrivateKey25519Companion.sign(privKey, messageToSign.getBytes).signature).asJson
          ).asJson)
        }
      }
    }
  }

}