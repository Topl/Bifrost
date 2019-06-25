package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.exceptions.JsonParsingException
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.transaction._
import bifrost.transaction.box.ProfileBox
import bifrost.wallet.BWallet
import io.circe.optics.JsonPath._
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{HCursor, Json}
import io.swagger.annotations._
import javax.ws.rs.Path
import bifrost.LocalInterface.LocallyGeneratedTransaction
import bifrost.settings.Settings
import bifrost.transaction.bifrostTransaction.{ProgramCreation, ProgramMethodExecution, ProfileTransaction}
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
  * Created by cykoz on 5/26/2017.
  */

@Path("/program")
@Api(value = "/program", produces = "application/json")
case class ProgramApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef, networkControllerRef: ActorRef)
                          (implicit val context: ActorRefFactory) extends ApiRouteWithView with ScorexLogging {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool

  override val route: Route = pathPrefix("program") {
    programRoute
  }

  //noinspection ScalaStyle
  def programRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            var reqId = ""
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(request) =>
                val futureResponse: Try[Future[Json]] = Try {
                  reqId = (request \\ "id").head.asString.get
                  require((request \\ "jsonrpc").head.asString.get == "2.0")
                  val params = (request \\ "params").head.asArray.get
                  require(params.size <= 5, s"size of params is ${params.size}")

                  (request \\ "method").head.asString.get match {
                    case "declareRole" => declareRole(params, reqId)
                    case "getRole" => getRole(params, reqId)
                    case "getProgramSignature" => getProgramSignature(params.head, reqId)
                    case "createProgram" => createProgram(params.head, reqId)
                    case "executeProgramMethod" => executeProgramMethod(params.head, reqId)
                    case "filter" => bloomFilter(params, reqId)
                  }
                }

                futureResponse map {
                  response => Await.result(response, timeout.duration)
                } match {
                  case Success(resp) => BifrostSuccessResponse(resp, reqId)
                  case Failure(e) =>
                    BifrostErrorResponse(e, 500, reqId)
                }
            }
          }
        }
      }
  }}

  def declareRole(params: Vector[Json], id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      params.map { param =>
        // parse the check for semantic validity
        val pubKey = (param \\ "publicKey").head.asString.get
        require(Base58.decode(pubKey).get.length == Curve25519.KeyLength)
        val pubKeyProp = PublicKey25519Proposition(Base58.decode(pubKey).get)
        val roleValue = (param \\ "role").head.asString.get
        require(ProfileBox.acceptableRoleValues.contains(roleValue))
        // Get the PrivateKey
        val privKeySet = wallet.secrets.filter(secret => secret.publicImage.pubKeyBytes sameElements Base58.decode(pubKey).get)
        require(privKeySet.nonEmpty, "Cannot Find an unlocked privateKey")
        // create Transaction
        val timestamp = System.currentTimeMillis()
        val signature = PrivateKey25519Companion.sign(privKeySet.toSeq.head,
          ProfileTransaction.messageToSign(timestamp, pubKeyProp,
            Map("role" -> roleValue)))
        val tx = ProfileTransaction(pubKeyProp, signature, Map("role" -> roleValue), 0L, timestamp)
        nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ProfileTransaction](tx)
        tx.json
      }.asJson
    }
  }

  def getRole(params: Vector[Json], id: String): Future[Json] = {
    viewAsync().map { view =>
      val state = view.state
      params.map { param =>
        val pubKey = (param \\ "publicKey").head.asString.get
        val prop = PublicKey25519Proposition(Base58.decode(pubKey).get)
        val box = state.closedBox(ProfileBox.idFromBox(prop, "role")).get
        box.json
      }.asJson
    }
  }

  def getProgramSignature(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val state = view.state
      val tx = createProgramInstance(params, state)
      val signature = PrivateKey25519Companion.sign(selectedSecret, tx.messageToSign)
      Map("signature" -> Base58.encode(signature.signature).asJson,
        "tx" -> tx.json.asJson).asJson
    }
  }

  def createProgram(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val state = view.state
      val tx = createProgramInstance(params, state)
      ProgramCreation.validate(tx) match {
        case Success(e) => log.info("Program creation validated successfully")
        case Failure(e) => throw e
      }
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ProgramCreation](tx)
      tx.json
    }
  }

  def executeProgramMethod(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
      val modifiedParams: Json = replaceBoxIdWithBox(view.state, params, "programBox")
      val cme = try{
        modifiedParams.as[ProgramMethodExecution]
      } catch {
        case e: Exception => e.getCause
      }
      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val tempTx = modifiedParams.as[ProgramMethodExecution] match {
        case Right(c: ProgramMethodExecution) => c
        case Left(e) => throw new JsonParsingException(s"Could not parse ProgramMethodExecution: $e")
      }
      val realSignature = PrivateKey25519Companion.sign(selectedSecret, tempTx.messageToSign)
      val tx = tempTx.copy(signatures = Map(PublicKey25519Proposition(Base58.decode(signingPublicKey).get) -> realSignature))
      ProgramMethodExecution.validate(tx) match {
        case Success(e) => log.info("Program method execution successfully validated")
        case Failure(e) => throw e.getCause
      }
      tx.newBoxes.toSet
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ProgramMethodExecution](tx)
      tx.json
    }
  }

  def bloomFilter(params: Vector[Json], id: String): Future[Json] = {
    viewAsync().map { view =>
      val history = view.history
      val queryBloomTopics = params.map(j => j.asString.getOrElse("")).map(s => Base58.decode(s).get)
      val res = history.bloomFilter(queryBloomTopics)
      res.map(_.json).asJson
    }
  }

  private def replaceBoxIdWithBox(state: BifrostState, json: Json, fieldName: String): Json = {
    val boxId = (json \\ fieldName).head.asString.get
    val boxInstance = state.closedBox(Base58.decode(boxId).get)

    val cursor: HCursor = json.hcursor
    cursor.downField(fieldName).withFocus(b => boxInstance.get.json).top.get
  }

  //noinspection ScalaStyle
  def createProgramInstance(json: Json, state: BifrostState): ProgramCreation = {
    json.as[ProgramCreation] match {
      case Right(c: ProgramCreation) => c
      case Left(e) => throw new JsonParsingException(s"Could not parse ProgramCreation: $e")
    }
  }
}