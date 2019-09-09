package bifrost.transaction.bifrostTransaction

import java.util.UUID

import bifrost.program.Program
import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.Nonce
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.srb.SBR
import bifrost.state.BifrostState
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.ProgramMethodExecutionCompanion
import bifrost.transaction.state.PrivateKey25519
import com.google.common.primitives.{Bytes, Longs}
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

case class ProgramMethodExecution(stateBox: StateBox,
                                  codeBox: CodeBox,
                                  executionBox: ExecutionBox,
                                  methodName: String,
                                  parameters: Json,
                                  owner: PublicKey25519Proposition,
                                  signatures: Map[PublicKey25519Proposition, Signature25519],
                                  preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                                  fees: Map[PublicKey25519Proposition, Long],
                                  timestamp: Long,
                                  data: String)
  extends ProgramTransaction {

  override type M = ProgramMethodExecution

  val proposition = executionBox.proposition


  // TODO Fix instantiation to handle runtime input and/or extract to a better location
  val forgingSettings = new ForgingSettings {
    override def settingsJSON: Map[String, Json] = super.settingsFromFile("testSettings.json")
  }

  //TODO do not readOrGenerate sbr here
  //SBR should be taken from nodeView at api level and passed as parameter to static function in companion object
  //Static function should extract necessary boxes and use those as parameters to transaction class
  //See static create function in companion object below

  val history = BifrostHistory.readOrGenerate(forgingSettings)
  val sbr: SBR = SBR.readOrGenerate(forgingSettings, history.storage.storage).get

  val uuidStateBoxes = executionBox.stateBoxUUIDs.map(v => sbr.getBox(v).get.asInstanceOf[StateBox]).zip(executionBox.stateBoxUUIDs)

  val codeBoxes = executionBox.codeBoxIds

  lazy val stateBoxIds: IndexedSeq[Array[Byte]] = IndexedSeq(stateBox.id)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = feeBoxIdKeyPairs.map(_._1)

  override lazy val unlockers: Traversable[BoxUnlocker[ProofOfKnowledgeProposition[PrivateKey25519]]] =
    Seq(new BoxUnlocker[PublicKey25519Proposition] {
    override val closedBoxId: Array[Byte] = executionBox.id
    override val boxKey: Signature25519 = signatures.getOrElse(owner, throw new Exception("Signature not provided"))
  }) ++ feeBoxUnlockers

  lazy val hashNoNonces = FastCryptographicHash(
    executionBox.id ++
      methodName.getBytes ++
      owner.pubKeyBytes ++
      parameters.noSpaces.getBytes ++
      unlockers.flatMap(_.closedBoxId) ++
      Longs.toByteArray(timestamp) ++
      fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
  )

  override lazy val newBoxes: Traversable[BifrostBox] = {
//    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val digest = FastCryptographicHash(proposition.pubKeyBytes ++ hashNoNonces)

    val nonce = ProgramTransaction.nonceFromDigest(digest)

    val programResult: Json = Program.execute(uuidStateBoxes, Seq(codeBox), methodName)(owner)(parameters.asObject.get)

    val updatedStateBox: StateBox = StateBox(signatures.head._1, nonce, uuidStateBoxes.head._1.value, programResult)

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

  //noinspection ScalaStyle
  def create(sbr: SBR,
             uuid: UUID,
             methodName: String,
             parameters: Json,
             owner: PublicKey25519Proposition,
             signatures: Map[PublicKey25519Proposition, Signature25519],
             preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
             fees: Map[PublicKey25519Proposition, Long],
             timestamp: Long,
             data: String): Try[ProgramMethodExecution] = Try {
    val execBox = sbr.getBox(uuid).get.asInstanceOf[ExecutionBox]
    val stateBox = sbr.getBox(execBox.stateBoxUUIDs.head).get.asInstanceOf[StateBox]
    val codeBox = sbr.getBox(UUID.nameUUIDFromBytes(execBox.codeBoxIds.head)).get.asInstanceOf[CodeBox]
    ProgramMethodExecution(stateBox, codeBox, execBox, methodName, parameters, owner, signatures, preFeeBoxes, fees, timestamp, data)
  }

  def validate(tx: ProgramMethodExecution): Try[Unit] = Try {

    require(tx.signatures(tx.owner).isValid(tx.owner, tx.messageToSign)
      , "Either an invalid signature was submitted or the party listed was not part of the program.")

  }.flatMap(_ => ProgramTransaction.commonValidation(tx))

  implicit val decodeProgramMethodExecution: Decoder[ProgramMethodExecution] = (c: HCursor) => for {
    stateBox <- c.downField("stateBox").as[StateBox]
    codeBox <- c.downField("codeBox").as[CodeBox]
    executionBox <- c.downField("executionBox").as[ExecutionBox]
    methodName <- c.downField("methodName").as[String]
    methodParams <- c.downField("methodParams").as[Json]
    rawOwner <- c.downField("owner").as[String]
    rawSignatures <- c.downField("signatures").as[Map[String, String]]
    rawPreFeeBoxes <- c.downField("preFeeBoxes").as[Map[String, IndexedSeq[(Long, Long)]]]
    rawFees <- c.downField("fees").as[Map[String, Long]]
    timestamp <- c.downField("timestamp").as[Long]
    data <- c.downField("data").as[String]
  } yield {
    val commonArgs = ProgramTransaction.commonDecode(rawOwner, rawSignatures, rawPreFeeBoxes, rawFees)
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