package bifrost.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.contract.{Agreement, AgreementTerms, PiecewiseLinearMultiple, PiecewiseLinearSingle}
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.transaction.ContractCreation._
import bifrost.scorexMod.{GenericBox, GenericBoxTransaction, GenericNodeViewHolder}
import bifrost.scorexMod.GenericNodeViewHolder.CurrentView
import bifrost.state.BifrostState
import bifrost.transaction._
import bifrost.transaction.box.{ContractBox, ProfileBox, ReputationBox}
import bifrost.wallet.BWallet
import io.circe.{HCursor, Json, JsonObject}
import io.circe.parser.parse
import io.circe.optics.JsonPath._
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.PersistentNodeViewModifier
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition, PublicKey25519Proposition}
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

/**
  * Created by cykoz on 5/26/2017.
  */

@Path("/contract")
@Api(value = "/contract", produces = "application/json")
case class ContractApiRoute (override val settings: Settings, nodeViewHolderRef: ActorRef)
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
                case "declareRole" => params.map(declareRole(view, _, id)).asJson
                case "getRole" => params.map(getRole(view, _, id)).asJson
                case "getContractSignature" => getContractSignature(view, params.head, id).asJson
                case "createContract" => createContract(view, params.head, id).asJson
                case "executeContractMethod" => executeContractMethod(view, params.head, id).asJson
                case "getCompletionSignature" => getCompletionSignature(view, params.head, id).asJson
                case "completeContract" => completeContract(view, params.head, id).asJson
                case "filter" => bloomFilter(view, params, id).asJson
              }
            } match {
              case Success(resp) => BifrostSuccessResponse(resp, reqId)
              case Failure(e) =>
                BifrostErrorResponse(e, reqId)
            }
          }
        }
      }
    }
  }}

  def declareRole(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val wallet = view.vault
    // parse the check for semantic validity
    val pubKey = (params \\ "publicKey").head.asString.get
    require(Base58.decode(pubKey).get.length == Curve25519.KeyLength)
    val pubKeyProp = PublicKey25519Proposition(Base58.decode(pubKey).get)
    val roleValue = (params \\ "role").head.asString.get
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
  }

  def getRole(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val state = view.state
    val pubKey = (params \\ "publicKey").head.asString.get
    val prop = PublicKey25519Proposition(Base58.decode(pubKey).get)
    val box = state.closedBox(ProfileBox.idFromBox(prop, "role")).get
    box.json
  }

  def getContractSignature(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val wallet = view.vault
    val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
    val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
    val state = view.state
    val tx = createContractInstance(params, state)
    val signature = PrivateKey25519Companion.sign(selectedSecret, tx.messageToSign)
    Map("signature" -> Base58.encode(signature.signature).asJson,
      "tx" -> tx.json.asJson).asJson
  }

  def createContract(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val state = view.state
    val tx = createContractInstance(params, state)
    ContractCreation.validate(tx) match {
      case Success(e) => log.info("Contract creation validated successfully")
      case Failure(e) => throw e
    }
    nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractCreation](tx)
    tx.json
  }

  def executeContractMethod(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
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

  def getCompletionSignature(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
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

  def completeContract(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
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

  def bloomFilter(view: CurrentView[HIS, MS, VL, MP], params: Vector[Json], id: String): Json = {
    val history = view.history
    val queryBloomTopics = params.map(j => j.asString.getOrElse("")).map(s => Base58.decode(s).get)
    val res = history.bloomFilter(queryBloomTopics)
    res.map(_.json).asJson
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