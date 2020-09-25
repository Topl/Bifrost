package bifrost.modifier.transaction.bifrostTransaction

import java.util.UUID

import bifrost.crypto.{FastCryptographicHash, MultiSignature25519, PrivateKey25519, Signature25519}
import bifrost.exceptions.TransactionValidationException
import bifrost.history.History
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.modifier.box.serialization.ExecutionBoxSerializer
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.ProgramMethodExecutionSerializer
import bifrost.program.Program
import bifrost.settings.AppSettings
import bifrost.state.{ProgramBoxRegistry, ProgramId, State, StateReader}
import bifrost.utils.serialization.BifrostSerializer
import com.google.common.primitives.{Bytes, Longs}
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

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

  val proposition: PublicKey25519Proposition = executionBox.proposition


  // TODO Fix instantiation to handle runtime input and/or extract to a better location
  val config: Config = ConfigFactory.load("application")
  val settings: AppSettings = AppSettings.fromConfig(config)

  //TODO do not readOrGenerate programBoxRegistry here
  //ProgramBoxRegistry should be taken from nodeView at api level and passed as parameter to static function in companion object
  //Static function should extract necessary boxes and use those as methodParams to transaction class
  //See static create function in companion object below

  val history: History = History.readOrGenerate(settings)
  val pbr: ProgramBoxRegistry = ProgramBoxRegistry.readOrGenerate(settings, history.storage.storage).get

  //val uuidStateBoxes = executionBox.stateBoxUUIDs.map(v => programBoxRegistry.getBox(v).get.asInstanceOf[StateBox])

  val codeBoxes: Seq[ProgramId] = executionBox.codeBoxIds

  //lazy val stateBoxIds: IndexedSeq[Array[Byte]] = IndexedSeq(state.head._1.id)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = feeBoxIdKeyPairs.map(_._1)

  //TODO deprecate timestamp once fee boxes are included in nonce generation
  lazy val hashNoNonces: FastCryptographicHash.Digest = FastCryptographicHash(
    executionBox.id ++
      methodName.getBytes ++
      owner.pubKeyBytes ++
      methodParams.noSpaces.getBytes ++
      Longs.toByteArray(timestamp) ++
      fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
  )

  override lazy val newBoxes: Traversable[StateBox] = {
    //    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val digest = FastCryptographicHash(proposition.pubKeyBytes ++ hashNoNonces)

    val nonce = ProgramTransaction.nonceFromDigest(digest)

    val programResult: Json =
      try {
        Program.execute(state, code, methodName)(owner)(methodParams.asObject.get)
      } catch {
        case e: Exception => throw e.getCause
      }

    //val programResult: Json = Program.execute(state, code, methodName)(owner)(methodParams.asObject.get)

    // enforces that the only editable state box is the first state box
    val updatedStateBox: StateBox = StateBox(owner, nonce, state.head.value, programResult)

    IndexedSeq(updatedStateBox) //++ deductedFeeBoxes(hashNoNonces)
  }

  lazy val json: Json = (commonJson.asObject.get.toMap ++ Map(
    "state" -> state.map {_.json}.asJson,
    "code" -> code.map {_.json}.asJson,
    "methodName" -> methodName.asJson,
    "methodParams" -> methodParams,
    "newBoxes" -> newBoxes.map {_.json}.toSeq.asJson
  )).asJson

  override lazy val serializer: BifrostSerializer[ProgramMethodExecution] = ProgramMethodExecutionSerializer

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

  type SR = StateReader[Box, ProofOfKnowledgeProposition[PrivateKey25519], Any]

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

  def syntacticValidate(tx: ProgramMethodExecution, withSigs: Boolean = true): Try[Unit] = Try {
    require(tx.signatures(tx.owner).isValid(tx.owner, tx.messageToSign)
      , "Either an invalid signature was submitted or the party listed was not part of the program.")
  }.flatMap(_ => ProgramTransaction.commonValidation(tx))

  def validatePrototype(tx: ProgramMethodExecution): Try[Unit] = syntacticValidate(tx, withSigs = false)

  def semanticValidate(tx: ProgramMethodExecution, state: SR): Try[Unit] = {
    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _ => // continue processing
    }

    /* Program exists */
    if (state.closedBox(tx.executionBox.id).isEmpty) {
      throw new TransactionValidationException(s"Program ${Base58.encode(tx.executionBox.id)} does not exist")
    }

    //TODO get execution box from box registry using UUID before using its actual id to get it from storage
    val executionBox: ExecutionBox = state.closedBox(tx.executionBox.id).get.asInstanceOf[ExecutionBox]
    val programProposition: PublicKey25519Proposition = executionBox.proposition

    /* This person belongs to program */
    if (!MultiSignature25519(tx.signatures.values.toSet).isValid(programProposition, tx.messageToSign)) {
      throw new TransactionValidationException(s"Signature is invalid for ExecutionBox")
    }

    // make sure we are not attempting to change an already deployed program
    if (tx.newBoxes.forall(curBox => state.closedBox(curBox.id).isDefined)) {
      Failure(new TransactionValidationException("ProgramCreation attempts to overwrite existing program"))
    }

    // check that the provided signatures generate valid unlockers
    val unlockers = State.generateUnlockers(tx.boxIdsToOpen, tx.signatures.head._2)
    unlockers
      .foldLeft[Try[Unit]](Success(()))((tryUnlock, unlocker) =>
        tryUnlock
          .flatMap { _ =>
            state.closedBox(unlocker.closedBoxId) match {
              case Some(box) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) => Success(Unit)
              case _ => Failure(new TransactionValidationException(s"Invalid transaction"))
            }
          }
      )
  }

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