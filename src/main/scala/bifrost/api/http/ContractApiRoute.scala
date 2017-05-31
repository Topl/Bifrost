package bifrost.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.contract.{Agreement, AgreementTerms, PiecewiseLinearMultiple, PiecewiseLinearSingle}
import bifrost.transaction.{ContractCreation, PolyTransfer, ProfileTransaction, Role}
import bifrost.transaction.box.ProfileBox
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.proof.Signature25519
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
    declareRole ~ getRole ~ createContract ~ getContractSignature
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
  def declareRole: Route = path("roles") { entity(as[String]) { body =>
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
              require(privKeySet.nonEmpty)
              // create Transaction
              val timestamp = System.currentTimeMillis()
              val signature = PrivateKey25519Companion.sign(privKeySet.toSeq.head,
                  ProfileTransaction.messageToSign(timestamp, pubKeyProp,
                  Map("role" -> roleValue)))
              val tx = ProfileTransaction(pubKeyProp, signature, Map("role" -> roleValue), 0L, timestamp)
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

  @Path("/roles/{pubKey}")
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
          println(s"Get Role Box, ${Base58.decode(pubKey).get}")
          val box = state.closedBox(FastCryptographicHash(Base58.decode(pubKey).get ++ "role".getBytes)).get

          SuccessApiResponse(box.json.asJson)
        }
      }
    }
  }

  def createContract: Route = path("create") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map { view =>
          parse(body) match {
            case Left(failure) => ApiException(failure.getCause)
            case Right(json) => Try {
              val tx = createContractInstance(json)
              nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractCreation](tx)
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

  def getContractSignature: Route = path("signatures") {entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map { view =>
          parse(body) match {
            case Left(failure) => ApiException(failure.getCause)
            case Right(json) => Try {
              val wallet = view.vault
              val secrets = wallet.secrets
              val signingPublicKey = (json \\ "signingPublicKey").head.asString.get
              val selectedSecret = secrets.toSeq.filter(p =>
                p.publicImage.pubKeyBytes sameElements Base58.decode(signingPublicKey).get
              ).head
              val tx = createContractInstance(json)
              val signature = PrivateKey25519Companion.sign(selectedSecret, tx.messageToSign)
              Map("signature" -> Base58.encode(signature.signature).asJson,
                "tx" -> tx.json.asJson).asJson
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

  def createContractInstance(json: Json): ContractCreation = {
    val agreement = (json \\ "agreement").head
    val pledge: Long = (agreement \\ "pledge").head.asNumber.get.toLong.get
    val xrate: BigDecimal = BigDecimal((agreement \\ "xrate").head.asNumber.get.toString)
    val shareFunc = new PiecewiseLinearMultiple(
      (agreement \\ "share").head.as[Seq[(Double,(Double, Double, Double))]].right.get
    )
    val fulfillFunc = new PiecewiseLinearSingle(
      (agreement \\ "fulfill").head.as[Seq[(Long, Double)]].right.get
    )
    val expiration: Long = (agreement \\ "expirationTime").head.asNumber.get.toLong.get
    val effectiveDate: Long = (agreement \\ "effectiveTime").head.asNumber.get.toLong.get
    val terms = new AgreementTerms(pledge, xrate, shareFunc, fulfillFunc)
    val contractAgreement = new Agreement(terms, effectiveDate, expiration)

    val hub = (json \\ "hub").head
    val producer = (json \\ "producer").head
    val investor = (json \\ "investor").head
    val fee: Long = (json \\ "fee").head.asNumber.get.toLong.get
    val timestamp: Long = (json \\ "timestamp").head.asNumber.get.toLong.get
    val hubPublicKey = PublicKey25519Proposition(Base58.decode((hub \\ "publicKey").head.asString.get).get)
    val hubSignature = Signature25519(Base58.decode((hub \\ "signature").head.asString.get).getOrElse(Array()))

    val producerPublicKey = PublicKey25519Proposition(Base58.decode((producer \\ "publicKey").head.asString.get).get)
    val producerSignature = Signature25519(Base58.decode((producer \\ "signature").head.asString.get).getOrElse(Array()))

    val investorPublicKey = PublicKey25519Proposition(Base58.decode((investor \\ "publicKey").head.asString.get).get)
    val investorSignature = Signature25519(Base58.decode((investor \\ "signature").head.asString.get).getOrElse(Array()))

    ContractCreation(contractAgreement,
      IndexedSeq((Role.Hub, hubPublicKey), (Role.Producer, producerPublicKey), (Role.Investor, investorPublicKey)),
      IndexedSeq(hubSignature, producerSignature, investorSignature),
      fee,
      System.currentTimeMillis()
    )
  }
}