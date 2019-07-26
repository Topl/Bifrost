package bifrost.transaction.bifrostTransaction

import java.util.UUID

import bifrost.program.Program
import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.Nonce
import Role.Role
import bifrost.BifrostApp
import bifrost.forging.ForgingSettings
import bifrost.srb.{SBR, StateBoxRegistry}
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.{MofNProposition, MofNPropositionSerializer, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.{MultiSignature25519, Proof, Signature25519}
import bifrost.transaction.serialization.ProgramMethodExecutionCompanion
import bifrost.transaction.state.PrivateKey25519
import com.google.common.primitives.{Bytes, Longs}
import io.circe
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.{Failure, Success, Try}

case class ProgramMethodExecution(stateBox: StateBox,
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
  extends ProgramTransaction {

  override type M = ProgramMethodExecution

  // lazy val proposition = MofNProposition(1, program.parties.map(p => p._1.pubKeyBytes).toSet)
//  val proposition = MofNProposition(1, executionBox.proposition.setOfPubKeyBytes)
val proposition = executionBox.proposition


  // TODO Fix instantiation to handle runtime input and/or extract to a better location
  val forgingSettings = new ForgingSettings {
    override def settingsJSON: Map[String, Json] = super.settingsFromFile("testSettings.json")
  }

  //TODO do not readOrGenerate sbr here
  //SBR should be taken from nodeView at api level and passed as parameter to static function in companion object
  //Static function should extract necessary boxes and use those as parameters to transaction class
  //See static create function in companion object below
  val sbr: StateBoxRegistry = StateBoxRegistry.readOrGenerate(forgingSettings)

  val uuidStateBoxes = executionBox.stateBoxUUIDs.map(v => sbr.get(v).get._2.asInstanceOf[StateBox]).zip(executionBox.stateBoxUUIDs)

  val codeBoxes = executionBox.codeBoxIds

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
//    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val digest = FastCryptographicHash(proposition.pubKeyBytes ++ hashNoNonces)

    val nonce = ProgramTransaction.nonceFromDigest(digest)

    val programResult: Json = Program.execute(uuidStateBoxes, Seq(codeBox), methodName)(parties.toIndexedSeq(0)._1)(parameters.asObject.get)

    val updatedStateBox: StateBox = StateBox(signatures.head._1, nonce, uuidStateBoxes.head._1.value, programResult, true)

      IndexedSeq(updatedStateBox) ++ deductedFeeBoxes(hashNoNonces)
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "stateBox" -> stateBox.json,
    "codeBox" -> codeBox.json,
    "methodName" -> methodName.asJson,
    "methodParams" -> parameters
  )).asJson

  override lazy val serializer = ProgramMethodExecutionCompanion

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    FastCryptographicHash(executionBox.bytes ++ hashNoNonces),
    data.getBytes
  )

  def assetNonce(prop: PublicKey25519Proposition, hashNoNonces: Array[Byte]): Nonce = ProgramTransaction
  .nonceFromDigest(
    FastCryptographicHash("assetNonce".getBytes
      ++ prop.pubKeyBytes
      ++ hashNoNonces)
  )

  override def toString: String = s"ProgramMethodExecution(${json.noSpaces})"
}

object ProgramMethodExecution {

  //YT NOTE - example of how to use static function to construct parameters for PME tx
  //YT NOTE - codeBoxIds in execution box should be changed to UUIDs given their inclusion in Program Registry
  def create(sbr: SBR,
             uuid: UUID,
             methodName: String,
             parameters: Json,
             parties: Map[PublicKey25519Proposition, Role],
             signatures: Map[PublicKey25519Proposition, Signature25519],
             preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
             fees: Map[PublicKey25519Proposition, Long],
             timestamp: Long,
             data: String): Try[ProgramMethodExecution] = Try {
    val execBox = sbr.getBox(uuid).get.asInstanceOf[ExecutionBox]
    val stateBox = sbr.getBox(execBox.stateBoxUUIDs.head).get.asInstanceOf[StateBox]
    val codeBox = sbr.getBox(UUID.nameUUIDFromBytes(execBox.codeBoxIds.head)).get.asInstanceOf[CodeBox]
    ProgramMethodExecution(stateBox, codeBox, execBox, methodName, parameters, parties, signatures, preFeeBoxes, fees, timestamp, data)
  }

  def validate(tx: ProgramMethodExecution): Try[Unit] = Try {

    require(tx.parties forall { case (proposition, _) =>
      tx.signatures(proposition).isValid(proposition, tx.messageToSign) &&
        MultiSignature25519(Set(tx.signatures(proposition))).isValid(tx.executionBox.proposition, tx.messageToSign)
    }, "Either an invalid signature was submitted or the party listed was not part of the program.")

    require(tx.parties.size == 1, "An incorrect number (not equal to 1) of parties provided signatures.")

  }.flatMap(_ => ProgramTransaction.commonValidation(tx))

  implicit val decodeProgramMethodExecution: Decoder[ProgramMethodExecution] = (c: HCursor) => for {
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
    val commonArgs = ProgramTransaction.commonDecode(rawParties, rawSignatures, rawPreFeeBoxes, rawFees)
    ProgramMethodExecution(
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