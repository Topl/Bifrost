package bifrost.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.transaction.{PolyTransfer, ProfileTransaction, Role}
import bifrost.transaction.box.ProfileBox
import io.circe.parser.parse
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.annotation.meta.field
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

/**
  * Created by cykoz on 5/26/2017.
  */

@Path("/contract")
@Api(value = "/contract", produces = "application/json")
case class ContractApiRoute (override val settings: Settings, nodeViewHolderRef: ActorRef)
                            (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  override val route: Route = pathPrefix("contract") {
    declareRole ~ getRole
  }

  @Path("/role")
  @ApiOperation(value = "role",
    notes = "Declare an address' role to the network",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      dataType = "String",
      paramType = "body",
      defaultValue = "{\n\t\"role\":\"hub\",\n\t\"publicKey\":\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"\n}"
    )
  ))
  def declareRole: Route = path("role") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map { view =>
          parse(body) match {
            case Left(failure) => ApiException(failure.getCause)
            case Right(json) => Try {
              val wallet = view.vault
              // parse the check for semantic validity
              val pubKey = (json \\ "publicKey").head.asString.get
              require(Base58.decode(pubKey).get.length == Curve25519.KeyLength)
              val pubKeyProp = PublicKey25519Proposition(Base58.decode(pubKey).get)
              val roleValue = (json \\ "role").head.asString.get
              require(ProfileBox.acceptableRoleValues.contains(roleValue))
              // Get the PrivateKey
              val privKeySet = wallet.secrets.filter(secret => secret.publicImage.pubKeyBytes sameElements Base58.decode(pubKey).get)
              println(s"privKeyset is ${privKeySet}")
              require(privKeySet.nonEmpty)
              // create Transaction
              val timestamp = System.currentTimeMillis()
              val signature = PrivateKey25519Companion.sign(privKeySet.toSeq.head,
                  ProfileTransaction.messageToSign(timestamp, pubKeyProp,
                  Map("role" -> roleValue)))
              val tx = ProfileTransaction(pubKeyProp, signature, Map("role" -> roleValue), 1L, timestamp)
              nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ProfileTransaction](tx)
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
  }}

  @Path("/role/{pubKey}")
  @ApiOperation(value = "role", notes = "Return info about a role associated with an account", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "pubKey",
      value = "Public Key to check in String format",
      required = true,
      dataType = "string",
      paramType = "path"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def getRole: Route = path("role" / Segment) { pubKey =>
    withAuth {
      getJsonRoute {
        viewAsync().map { view =>
          val state = view.state
          val box = state.closedBox(FastCryptographicHash(Base58.decode(pubKey).get ++ "role".getBytes)).get

          SuccessApiResponse(box.json.asJson)
        }
      }
    }
  }
}