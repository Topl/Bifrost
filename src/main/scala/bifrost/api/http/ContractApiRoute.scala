package bifrost.api.http

import java.time.Instant

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.exceptions.JsonParsingException
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.transaction.ContractCreation._
import bifrost.transaction._
import bifrost.transaction.box.ProfileBox
import bifrost.wallet.BWallet
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import io.circe.Decoder.Result
import io.circe.optics.JsonPath._
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import io.swagger.annotations._
import javax.ws.rs.Path
import bifrost.LocalInterface.LocallyGeneratedTransaction
import bifrost.api.http.ApiException
import bifrost.settings.Settings
import bifrost.transaction.bifrostTransaction.ContractCreation
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import scalapb.json4s.JsonFormat

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

/**
  * Created by cykoz on 5/26/2017.
  */

@Path("/contract")
@Api(value = "/contract", produces = "application/json")
case class ContractApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef, networkControllerRef: ActorRef)
                           (implicit val context: ActorRefFactory) extends ApiRouteWithView with ScorexLogging {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool

  override val route: Route = pathPrefix("contract") {
    contractRoute
  }

  //noinspection ScalaStyle
  def contractRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          //viewAsync().map { view =>
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
                  case "getContractSignature" => getContractSignature(params.head, reqId)
                  case "createContract" => createContract(params.head, reqId)
                  case "executeContractMethod" => executeContractMethod(params.head, reqId)
                  case "getCompletionSignature" => getCompletionSignature(params.head, reqId)
                  case "completeContract" => completeContract(params.head, reqId)
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
  }

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

  def getContractSignature(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      //println(s">>>>>>>>>>>>>>>>>>>> ${params} <<<<<<<<<<<<<<<<<<<<<<<<")
      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val state = view.state
      val tx = createContractInstance(params, state)
      val signature = PrivateKey25519Companion.sign(selectedSecret, tx.messageToSign)
      Map("signature" -> Base58.encode(signature.signature).asJson,
        "tx" -> tx.json.asJson).asJson
    }
  }

  def createContract(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val state = view.state
      val tx = createContractInstance(params, state)
      ContractCreation.validate(tx) match {
        case Success(e) => log.info("Contract creation validated successfully")
        case Failure(e) => throw e
      }
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractCreation](tx)
      tx.json
    }
  }

  def executeContractMethod(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault

      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get

      val modifiedParams: Json = replaceBoxIdWithBox(view.state, params, "contractBox")

      val cme = try{
        modifiedParams.as[ContractMethodExecution]
      } catch {
        case e: Exception => e.getCause
      }

      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val tempTx = modifiedParams.as[ContractMethodExecution] match {
        case Right(c: ContractMethodExecution) => c
        case Left(e) => throw new JsonParsingException(s"Could not parse ContractMethodExecution: $e")
      }

      val realSignature = PrivateKey25519Companion.sign(selectedSecret, tempTx.messageToSign)

      val tx = tempTx.copy(signatures = Map(PublicKey25519Proposition(Base58.decode(signingPublicKey).get) -> realSignature))

//      println(s"${tx.signatures.toString()}")
//      println(s"${tx.json}")

      ContractMethodExecution.validate(tx) match {
        case Success(e) => log.info("Contract method execution successfully validated")
        case Failure(e) => throw e.getCause
      }
      tx.newBoxes.toSet
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractMethodExecution](tx)
      tx.json
    }
  }

  def getCompletionSignature(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val state = view.state
      val wallet = view.vault
      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get

      val modified = replaceBoxIdWithBox(state, params, "contractBox")
      val modifiedParams = root.reputationBoxes.each.json.modify(
        id => {
          val boxInstance = state.closedBox(Base58.decode(id.asString.get).get)
          boxInstance.get.json
        }
      )(modified)
      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val tx = createCompletionInstance(modifiedParams, state)
      val signature = PrivateKey25519Companion.sign(selectedSecret, tx.messageToSign)
      Map("signature" -> Base58.encode(signature.signature).asJson,
        "tx" -> tx.json.asJson).asJson
    }
  }

  def completeContract(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val state = view.state
      val modified = replaceBoxIdWithBox(state, params, "contractBox")
      val modifiedParams = root.reputationBoxes.each.json.modify(
        id => {
          val boxInstance = state.closedBox(Base58.decode(id.asString.get).get)
          boxInstance.get.json
        }
      )(modified)
      val tx = createCompletionInstance(modifiedParams, state)
      ContractCompletion.validate(tx) match {
        case Success(e) => log.info("Contract completion successfully validated")
        case Failure(e) => throw e
      }
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractCompletion](tx)
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
  def createContractInstance(json: Json, state: BifrostState): ContractCreation = {
    json.as[ContractCreation] match {
      case Right(c: ContractCreation) => c
      case Left(e) => throw new JsonParsingException(s"Could not parse ContractCreation: $e")
    }
  }

  def createCompletionInstance(json: Json, state: BifrostState): ContractCompletion = {
    json.as[ContractCompletion] match {
      case Right(c: ContractCompletion) => c
      case Left(e) => throw new JsonParsingException(s"Could not parse ContractCompletion: $e")
    }
  }
}