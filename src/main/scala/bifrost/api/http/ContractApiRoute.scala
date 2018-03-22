package bifrost.api.http

import java.time.Instant
import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.network.{PeerMessageManager, PeerMessageSpec}
import bifrost.transaction.ContractCreation._
import bifrost.state.BifrostState
import bifrost.transaction._
import bifrost.transaction.box.{ContractBox, ProfileBox, ReputationBox}
import bifrost.wallet.BWallet
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString
import com.trueaccord.scalapb.json.JsonFormat
import io.circe.{HCursor, Json, JsonObject}
import io.circe.parser.parse
import io.circe.optics.JsonPath._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import serializer.{PeerMessage, ProducerProposal}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success, Try}
import scorex.core.network.message.Message

/**
  * Created by cykoz on 5/26/2017.
  */

@Path("/contract")
@Api(value = "/contract", produces = "application/json")
case class ContractApiRoute (override val settings: Settings, nodeViewHolderRef: ActorRef, networkControllerRef: ActorRef)
                            (implicit val context: ActorRefFactory) extends ApiRouteWithView with ScorexLogging {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool

  override val route: Route = pathPrefix("contract") {
    contractRoute
  }

  //noinspection ScalaStyle
  def contractRoute: Route = path("") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        //viewAsync().map { view =>
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
              case "declareRole" => declareRole(params, id)
              case "getRole" => getRole(params, id)
              case "getContractSignature" => getContractSignature(params.head, id)
              case "createContract" => createContract(params.head, id)
              case "executeContractMethod" => executeContractMethod(params.head, id)
              case "getCompletionSignature" => getCompletionSignature(params.head, id)
              case "completeContract" => completeContract(params.head, id)
              case "filter" => bloomFilter(params, id)
              case "retrieveProposals" => retrieveProposals(params.head, id)
              case "postProposals" => postProposals(params.head, id)
            }
          } map { resp =>
            Await.result(resp, timeout.duration)
          } match {
            case Success(resp) => BifrostSuccessResponse(resp, reqId)
            case Failure(e) =>
              BifrostErrorResponse(e, reqId)
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

  def getContractSignature(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
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
      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val tempTx = modifiedParams.as[ContractMethodExecution] match {
        case Right(c: ContractMethodExecution) => c
        case Left(e) => throw new Exception(s"Could not parse ContractMethodExecution: $e")
      }

      val realSignature = PrivateKey25519Companion.sign(selectedSecret, tempTx.messageToSign)
      val tx = tempTx.copy(signatures = Map(PublicKey25519Proposition(Base58.decode(signingPublicKey).get) -> realSignature))
      ContractMethodExecution.validate(tx) match {
        case Success(e) => log.info("Contract method execution successfully validated")
        case Failure(e) => throw e
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

  def retrieveProposals(params: Json, id: String): Future[Json] = {
    messageManagerAsync().map { messageManagerWrapper =>
      val messageManager = messageManagerWrapper.m
      val limit = (params \\ "limit").head.asNumber.get.toInt.getOrElse(50)
      val producerProposals = messageManager.filter(_.messageType equals PeerMessage.Type.ProducerProposal)
      val proposals = producerProposals.take(limit)
      Map(
        "proposals" -> proposals.map(proposal => parse(JsonFormat.toJsonString(proposal)).right.getOrElse(Json.Null)).asJson,
        "totalProposals" -> producerProposals.size.asJson
      ).asJson
    }
  }

  def postProposals(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>

      val timestamp = Instant.now.toEpochMilli
      val producerProposal = JsonFormat.fromJsonString[ProducerProposal](params.toString)

      val wrappedMessage = PeerMessage(
        PeerMessage.Type.ProducerProposal,
        ByteString.copyFrom(
          PrivateKey25519Companion.sign(view.vault.secrets.head, producerProposal.toByteArray ++ Longs.toByteArray(timestamp)).bytes
        ),
        timestamp,
        producerProposal.toByteString,
        ByteString.copyFrom(view.vault.secrets.head.publicImage.pubKeyBytes)
      )

      networkControllerRef ! Message(PeerMessageSpec, Left(wrappedMessage.toByteArray), Some(null))

      parse(JsonFormat.toJsonString(wrappedMessage)).right.getOrElse(Json.Null)
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
      case Left(e) => throw new Exception(s"Could not parse ContractCreation: $e")
    }
  }

  def createCompletionInstance(json: Json, state: BifrostState): ContractCompletion = {
    json.as[ContractCompletion] match {
      case Right(c: ContractCompletion) => c
      case Left(e) => throw new Exception(s"Could not parse ContractCompletion: $e")
    }
  }
}