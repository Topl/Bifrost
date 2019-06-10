package bifrost.transaction.bifrostTransaction

import bifrost.contract.Contract
import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.Nonce
import Role.Role
import bifrost.BifrostApp
import bifrost.forging.ForgingSettings
import bifrost.srb.StateBoxRegistry
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.{MultiSignature25519, Proof, Signature25519}
import bifrost.transaction.serialization.ContractMethodExecutionCompanion
import bifrost.transaction.state.PrivateKey25519
import com.google.common.primitives.{Bytes, Longs}
import io.circe
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.{Failure, Success, Try}

case class ContractMethodExecution(stateBox: StateBox,
                                   codeBox: CodeBox,
                                   executionBox: ExecutionBox,
                                   methodName: String,
                                   parameters: Json,
                                   parties: Map[PublicKey25519Proposition, Role],
                                   signatures: Map[PublicKey25519Proposition, Signature25519],
                                   preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                                   fees: Map[PublicKey25519Proposition, Long],
                                   timestamp: Long,
                                   data: String)
  extends ContractTransaction {

  override type M = ContractMethodExecution

  //lazy val proposition = MofNProposition(1, contract.parties.map(p => p._1.pubKeyBytes).toSet)
  val proposition = MofNProposition(1, executionBox.proposition.setOfPubKeyBytes)

  val forgingSettings = new ForgingSettings {
    override def settingsJSON: Map[String, Json] = super.settingsFromFile("testSettings.json")
  }

  val sbr: StateBoxRegistry = StateBoxRegistry.readOrGenerate(forgingSettings)

  val uuidStateBoxes = executionBox.value.map(v => sbr.get(v).get._2.asInstanceOf[StateBox])

  //val program: String = stateBox.value.foldLeft("")((a,b) => a ++ (b + "\n")) ++ codeBox.value.foldLeft("")((a,b) => a ++ (b + "\n"))
  val program: String = uuidStateBoxes.foldLeft("")((a,b) => a ++ b.value.foldLeft("")((a,b) => a ++ b)) ++ codeBox.value.foldLeft("")((a,b) => a ++ (b + "\n"))

  lazy val stateBoxIds: IndexedSeq[Array[Byte]] = IndexedSeq(stateBox.id)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = feeBoxIdKeyPairs.map(_._1)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] = Seq(
    new BoxUnlocker[MofNProposition] {
      override val closedBoxId: Array[Byte] = stateBoxIds.head
      override val boxKey: Proof[MofNProposition] = MultiSignature25519(parties.map(p => signatures.get(p._1) match {
        case Some(sig) => sig
        case None => Signature25519(Array[Byte]())
      }).toSet)
    }
  ) ++ feeBoxUnlockers

  lazy val hashNoNonces = FastCryptographicHash(
    executionBox.id ++
      methodName.getBytes ++
      parties.toSeq.sortBy(_._1.pubKeyBytes.mkString("")).foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes) ++
      parameters.noSpaces.getBytes ++
      unlockers.flatMap(_.closedBoxId) ++
      Longs.toByteArray(timestamp) ++
      fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
  )

  override lazy val newBoxes: Traversable[BifrostBox] = {
    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val nonce = ContractTransaction.nonceFromDigest(digest)

    val contractResult: String = Contract.execute(program, methodName)(parties.toIndexedSeq(0)._1)(parameters.asObject
                                                                                              .get) //match {
      /*case Success(res) => res match {
        /*case Left(updatedContract) => ContractBox(
          proposition,
          nonce,
          updatedContract.json)
        case Right(_) => contractBox
         */
      }
      //case Failure(_) => contractBox
    }*/

    //Handle boxes being sent from the contract to a public key
    /*val boxesFromContract: Option[BifrostBox] = methodName match {
      case "assetTransfer" => {
        val key = (parameters \\ "publicKey").head.asString.get
        val asset = (parameters \\ "asset").head.asString.get
        val amount = (parameters \\ "amount").head.asNumber.get.toLong.get
        if (key != "contract") {
          Some(AssetBox(PublicKey25519Proposition(key.getBytes),
            assetNonce(PublicKey25519Proposition(key.getBytes), hashNoNonces), amount, asset, parties.head._1, data))
        }
        else
          None
      }

      case "polyTransfer" => {
        val key = (parameters \\ "publicKey").head.asString.get
        val amount = (parameters \\ "amount").head.asNumber.get.toLong.get
        if(key != "contract"){
          Some(PolyBox(PublicKey25519Proposition(key.getBytes),
            assetNonce(PublicKey25519Proposition(key.getBytes), hashNoNonces), amount))
        }
        else
          None
      }

      case _ => None
    }

    if(boxesFromContract.nonEmpty)
      IndexedSeq(contractResult) ++ deductedFeeBoxes(hashNoNonces) :+ boxesFromContract.get
    else*/

    val updatedStateBox: StateBox = StateBox(signatures.head._1, nonce, Seq(contractResult), true)

      IndexedSeq(updatedStateBox) ++ deductedFeeBoxes(hashNoNonces)
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "stateBox" -> stateBox.json,
    "codeBox" -> codeBox.json,
    "methodName" -> methodName.asJson,
    "methodParams" -> parameters
  )).asJson

  override lazy val serializer = ContractMethodExecutionCompanion

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    FastCryptographicHash(executionBox.bytes ++ hashNoNonces),
    data.getBytes
  )

  def assetNonce(prop: PublicKey25519Proposition, hashNoNonces: Array[Byte]): Nonce = ContractTransaction
  .nonceFromDigest(
    FastCryptographicHash("assetNonce".getBytes
      ++ prop.pubKeyBytes
      ++ hashNoNonces)
  )

  override def toString: String = s"ContractMethodExecution(${json.noSpaces})"
}

object ContractMethodExecution {

  def validate(tx: ContractMethodExecution): Try[Unit] = Try {

    require(tx.parties forall { case (proposition, _) =>
      tx.signatures(proposition).isValid(proposition, tx.messageToSign) &&
        MultiSignature25519(Set(tx.signatures(proposition))).isValid(tx.executionBox.proposition, tx.messageToSign)
    }, "Either an invalid signature was submitted or the party listed was not part of the contract.")

    require(tx.parties.size == 1, "An incorrect number (not equal to 1) of parties provided signatures.")

  }.flatMap(_ => ContractTransaction.commonValidation(tx))

  implicit val decodeContractMethodExecution: Decoder[ContractMethodExecution] = (c: HCursor) => for {
    stateBox <- c.downField("stateBox").as[StateBox]
    codeBox <- c.downField("codeBox").as[CodeBox]
    executionBox <- c.downField("executionBox").as[ExecutionBox]
    methodName <- c.downField("methodName").as[String]
    methodParams <- c.downField("methodParams").as[Json]
    rawParties <- c.downField("parties").as[Map[String, String]]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ContractTransaction.commonDecode(rawParties, rawSignatures, rawPreFeeBoxes, rawFees)
    ContractMethodExecution(
      stateBox,
      codeBox,
      executionBox,
      methodName,
      methodParams,
      commonArgs._1,
      commonArgs._2,
      commonArgs._3,
      commonArgs._4,
      timestamp,
      data)
  }
}