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
import bifrost.transaction.ContractTransaction.Nonce
import bifrost.transaction._
import bifrost.transaction.box.{ContractBox, ProfileBox, ReputationBox}
import bifrost.wallet.BWallet
import io.circe.{Json, JsonObject}
import io.circe.parser.parse
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
                            (implicit val context: ActorRefFactory) extends ApiRouteWithView {
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
    require(privKeySet.nonEmpty)
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
    println(s"Get Role Box for public Key: ${pubKey}")
    val box = state.closedBox(ProfileBox.idFromBox(prop, "role")).get
    box.json
  }

  def getContractSignature(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val wallet = view.vault
    val secrets = wallet.secrets
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
      case Success(e) => println("Contract Creation validation success")
      case Failure(e) => throw e
    }
    nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractCreation](tx)
    tx.json
  }

  def executeContractMethod(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val wallet = view.vault
    val secrets = wallet.secrets
    val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
    val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
    val state = view.state
    val tempTx = params.as[ContractMethodExecution] match {
      case Right(c: ContractMethodExecution) => c
      case Left(e) => throw new Exception(s"Could not parse ContractMethodExecution: $e")
    }

    val realSignature = PrivateKey25519Companion.sign(selectedSecret, tempTx.messageToSign)
    val tx = tempTx.copy(signatures = Map(PublicKey25519Proposition(Base58.decode(signingPublicKey).get) -> realSignature))
    ContractMethodExecution.validate(tx) match {
      case Success(e) => println("validation success")
      case Failure(e) => throw e
    }
    nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractMethodExecution](tx)
    tx.json
  }

  def getCompletionSignature(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val state = view.state
    val wallet = view.vault
    val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
    val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
    val tx = createCompletionInstance(params, state)
    val signature = PrivateKey25519Companion.sign(selectedSecret, tx.messageToSign)
    Map("signature" -> Base58.encode(signature.signature).asJson,
      "tx" -> tx.json.asJson).asJson
  }

  def completeContract(view: CurrentView[HIS, MS, VL, MP], params: Json, id: String): Json = {
    val state = view.state
    val tx = createCompletionInstance(params, state)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ContractCompletion](tx)
    tx.json
  }

  //noinspection ScalaStyle
  def createContractInstance(json: Json, state: BifrostState): ContractCreation = {
    json.as[ContractCreation] match {
      case Right(c: ContractCreation) => c
      case Left(e) => throw new Exception(s"Could not parse ContractCreation: $e")
    }
  }

  def createCompletionInstance(json: Json, state: BifrostState): ContractCompletion = {
    val contractBoxId = (json \\ "contractBoxId").head.asString.get
    val contractBox = state.closedBox(Base58.decode(contractBoxId).get).get.asInstanceOf[ContractBox]
    val reputationBoxIds = (json \\ "reputationBoxIds").head.as[Array[String]].right.getOrElse(Array[String]())
    val reputationBoxes = reputationBoxIds.map(b => state.closedBox(Base58.decode(b).get).get.asInstanceOf[ReputationBox])
    val hub = (json \\ "hub").head
    val producer = (json \\ "producer").head
    val investor = (json \\ "investor").head
    val fees: Map[PublicKey25519Proposition, Long] = (json \\ "fees").head.asObject.map(_.toMap)
      .fold(Map[PublicKey25519Proposition, Long]())(_.map { case (prop: String, v: Json) =>
        (PublicKey25519Proposition(Base58.decode(prop).getOrElse(Array[Byte]())), v.asNumber.fold(0L)(_.toLong.getOrElse(0L)))
      })

    val feePreBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]] = (json \\ "feePreBoxes").head.asObject
      .map(_.toMap).fold(Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]]())(_.map {
      case (prop: String, v: Json) =>
        (PublicKey25519Proposition(Base58.decode(prop).getOrElse(Array[Byte]())),
          v.asArray.fold(IndexedSeq[(Long, Long)]())(_.map(preBox => {
            val nonceLongPair = preBox.asArray
            nonceLongPair.map(nlp =>
              (nlp(0).asNumber.fold(0L)(_.toLong.getOrElse(0L)), nlp(1).asNumber.fold(0L)(_.toLong.getOrElse(0L)))
            )
          }.getOrElse((0L, 0L))).toIndexedSeq
          )
        )
    })
    // extract publicKeys and signatures of all three actors
    val extractedTuple = ContractApiRoute.extractPublicKeyAndSignatures(hub, producer, investor)
    val Array(hubPublicKey, producerPublicKey, investorPublicKey) = extractedTuple._1
    val Array(hubSignature, producerSignature, investorSignature) = extractedTuple._2
    // create ContractCompletion tx
    ContractCompletion(contractBox, reputationBoxes.toIndexedSeq,
      Map(
        Role.Hub -> hubPublicKey,
        Role.Producer -> producerPublicKey,
        Role.Investor -> investorPublicKey
      ),
      Map(
        hubPublicKey -> hubSignature,
        producerPublicKey -> producerSignature,
        investorPublicKey -> investorSignature
      ),
      feePreBoxes,
      fees,
      System.currentTimeMillis())
  }
}

object ContractApiRoute {

  def extractPublicKeyAndSignatures(hub: Json, producer: Json, investor: Json):
    (Array[PublicKey25519Proposition], Array[Signature25519]) = {
    val hubPublicKey = PublicKey25519Proposition(Base58.decode((hub \\ "publicKey").head.asString.get).get)
    val hubSignature = Signature25519(Base58.decode((hub \\ "signature").head.asString.get).getOrElse(Array.fill(Curve25519.SignatureLength)(1.toByte)))

    val producerPublicKey = PublicKey25519Proposition(Base58.decode((producer \\ "publicKey").head.asString.get).get)
    val producerSignature = Signature25519(Base58.decode((producer \\ "signature").head.asString.get).getOrElse(Array.fill(Curve25519.SignatureLength)(1.toByte)))

    val investorPublicKey = PublicKey25519Proposition(Base58.decode((investor \\ "publicKey").head.asString.get).get)
    val investorSignature = Signature25519(Base58.decode((investor \\ "signature").head.asString.get).getOrElse(Array.fill(Curve25519.SignatureLength)(1.toByte)))
    (Array(hubPublicKey, producerPublicKey, investorPublicKey), Array(hubSignature, producerSignature, investorSignature))
  }

}