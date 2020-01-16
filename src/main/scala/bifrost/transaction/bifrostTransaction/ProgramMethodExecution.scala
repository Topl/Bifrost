package bifrost.transaction.bifrostTransaction

import java.util.UUID

import bifrost.program.Program
import bifrost.crypto.hash.FastCryptographicHash
import BifrostTransaction.Nonce
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.programBoxRegistry.ProgramBoxRegistry
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.serialization.ProgramMethodExecutionCompanion
import com.google.common.primitives.{Bytes, Longs}
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

case class ProgramMethodExecution(state: Seq[StateBox],
                                  code: Seq[CodeBox],
                                  executionBox: ExecutionBox,
                                  methodName: String,
                                  methodParams: Json,
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

  //TODO do not readOrGenerate programBoxRegistry here
  //ProgramBoxRegistry should be taken from nodeView at api level and passed as parameter to static function in companion object
  //Static function should extract necessary boxes and use those as methodParams to transaction class
  //See static create function in companion object below

  val history = BifrostHistory.readOrGenerate(forgingSettings)
  val pbr: ProgramBoxRegistry = ProgramBoxRegistry.readOrGenerate(forgingSettings, history.storage.storage).get

  //val uuidStateBoxes = executionBox.stateBoxUUIDs.map(v => programBoxRegistry.getBox(v).get.asInstanceOf[StateBox])

  val codeBoxes = executionBox.codeBoxIds

  //lazy val stateBoxIds: IndexedSeq[Array[Byte]] = IndexedSeq(state.head._1.id)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = feeBoxIdKeyPairs.map(_._1)

  lazy val hashNoNonces = FastCryptographicHash(
    executionBox.id ++
      methodName.getBytes ++
      owner.pubKeyBytes ++
      methodParams.noSpaces.getBytes ++
      Longs.toByteArray(timestamp) ++
      fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
  )

  override lazy val newBoxes: Traversable[BifrostBox] = {
//    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val digest = FastCryptographicHash(proposition.pubKeyBytes ++ hashNoNonces)

    val nonce = ProgramTransaction.nonceFromDigest(digest)

    try {
      Program.execute(state, code, methodName)(owner)(methodParams.asObject.get)
    } catch {
      case e: Exception => throw e.getCause
    }

    val programResult: Json = Program.execute(state, code, methodName)(owner)(methodParams.asObject.get)

    val updatedStateBox: StateBox = StateBox(owner, nonce, state.head.value, programResult)

    IndexedSeq(updatedStateBox) ++ deductedFeeBoxes(hashNoNonces)
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "state" -> state.map {
      sb => sb.json
    }.asJson,
    "code" -> code.map {
      cb => cb.json
    }.asJson,
    "methodName" -> methodName.asJson,
    "methodParams" -> methodParams,
    "newBoxes" -> newBoxes.map {
      nb => nb.json
    }.asJson
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

  //YT NOTE - example of how to use static function to construct methodParams for PME tx
  //YT NOTE - codeBoxIds in execution box should be changed to UUIDs given their inclusion in Program Registry

  //noinspection ScalaStyle
  def create(pbr: ProgramBoxRegistry,
             uuid: UUID,
             methodName: String,
             methodParams: Json,
             owner: PublicKey25519Proposition,
             signatures: Map[PublicKey25519Proposition, Signature25519],
             preFeeBoxes: Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
             fees: Map[PublicKey25519Proposition, Long],
             timestamp: Long,
             data: String): Try[ProgramMethodExecution] = Try {
    val execBox = pbr.getBox(uuid).get.asInstanceOf[ExecutionBox]
    val state: Seq[StateBox] = execBox.stateBoxUUIDs.map(sb => pbr.getBox(sb).get.asInstanceOf[StateBox])
    //val codeBox = programBoxRegistry.getBox(UUID.nameUUIDFromBytes(execBox.codeBoxIds.head)).get.asInstanceOf[CodeBox]
    val code: Seq[CodeBox] = execBox.codeBoxIds.map(cb => pbr.getBox(UUID.nameUUIDFromBytes(cb)).get.asInstanceOf[CodeBox])
    ProgramMethodExecution(state, code, execBox, methodName, methodParams, owner, signatures, preFeeBoxes, fees, timestamp, data)
  }

  def validate(tx: ProgramMethodExecution): Try[Unit] = Try {

    require(tx.signatures(tx.owner).isValid(tx.owner, tx.messageToSign)
      , "Either an invalid signature was submitted or the party listed was not part of the program.")

  }.flatMap(_ => ProgramTransaction.commonValidation(tx))

  implicit val decodeProgramMethodExecution: Decoder[ProgramMethodExecution] = (c: HCursor) => for {
    state <- c.downField("state").as[Seq[StateBox]]
    code <- c.downField("code").as[Seq[CodeBox]]
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
      state,
      code,
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