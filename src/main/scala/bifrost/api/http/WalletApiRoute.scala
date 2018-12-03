package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import bifrost.transaction.PolyTransfer
import bifrost.transaction.box.{ArbitBox, PolyBox}
import io.circe.parser._
import io.circe.syntax._
import io.swagger.annotations._
import javax.ws.rs.Path
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


@Path("/wallet")
@Api(value = "/wallet", produces = "application/json")
case class WalletApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  val DefaultFee = 100

  override implicit val timeout: Timeout = Timeout(10.seconds)

  override val route: Route = pathPrefix("wallet") {
    balances ~ transfer ~ generateKeyFile ~ unlockKeyFile
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
                val data: String = (json \\ "data").head.asString.get
                val tx = PolyTransfer.create(wallet, IndexedSeq((recipient, amount)), fee, data).get
                nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
                tx.json
              } match {
                case Success(resp) => SuccessApiResponse(resp)
                case Failure(e) =>
                  e.printStackTrace()
                  ApiException(e)
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

  @Path("/keyfile/")
  @ApiOperation(value = "Create a Keyfile", notes = "Create a Keyfile", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "password",
      value = "Password to lock the keyfile",
      required = true,
      dataType = "string",
      paramType = "body"
    )
  ))
  def generateKeyFile: Route = path("keyfile") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(json) => Try {
                val wallet = view.vault
                val password: String = (json \\ "password").head.asString.get
                val pubKey = wallet.generateNewSecret(password)
                Map(
                  "publicKey" -> Base58.encode(pubKey.pubKeyBytes).asJson
                ).asJson
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

  @Path("/unlock/")
  @ApiOperation(value = "Unlock a Keyfile", notes = "Unlock a Keyfile", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "password",
      value = "Password to lock the keyfile",
      required = true,
      dataType = "string",
      paramType = "body"
    )
  ))
  def unlockKeyFile: Route = path("unlock") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(json) => Try {
                val wallet = view.vault
                val publicKey: String = (json \\ "publicKey").head.asString.get
                val password: String = (json \\ "password").head.asString.get
                wallet.unlockKeyFile(publicKey, password)
                Map(
                  publicKey -> "unlocked".asJson
                ).asJson
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

}