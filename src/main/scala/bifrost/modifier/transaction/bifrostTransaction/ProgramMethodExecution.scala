package bifrost.modifier.transaction.bifrostTransaction

import bifrost.crypto.{ FastCryptographicHash, MultiSignature25519, PrivateKey25519, Signature25519 }
import bifrost.exceptions.TransactionValidationException
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{ ProofOfKnowledgeProposition, PublicKey25519Proposition }
import bifrost.modifier.transaction.bifrostTransaction.Transaction.Nonce
import bifrost.modifier.transaction.serialization.ProgramMethodExecutionSerializer
import bifrost.program.Program
import bifrost.state.{ ProgramBoxRegistry, ProgramId, State, StateReader }
import bifrost.utils.serialization.BifrostSerializer
import com.google.common.primitives.{ Bytes, Longs }
import io.circe.syntax._
import io.circe.{ Decoder, HCursor, Json }
import scorex.util.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.{ Failure, Success, Try }

case class ProgramMethodExecution ( executionBox: ExecutionBox,
                                    stateBoxes  : Seq[StateBox],
                                    codeBoxes   : Seq[CodeBox],
                                    methodName  : String,
                                    methodParams: Json,
                                    owner       : PublicKey25519Proposition,
                                    signatures  : Map[PublicKey25519Proposition, Signature25519],
                                    preFeeBoxes : Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
                                    fees        : Map[PublicKey25519Proposition, Long],
                                    timestamp   : Long,
                                    data        : String
                                  ) extends ProgramTransaction {

  override type M = ProgramMethodExecution

  val proposition: PublicKey25519Proposition = executionBox.proposition

  // TODO Fix instantiation to handle runtime input and/or extract to a better location
//  val config  : Config      = ConfigFactory.load("application")
//  val settings: AppSettings = AppSettings.fromConfig(config)

  //TODO do not readOrGenerate programBoxRegistry here
  //ProgramBoxRegistry should be taken from nodeView at api level and passed as parameter to static function in companion object
  //Static function should extract necessary boxes and use those as methodParams to transaction class
  //See static create function in companion object below

//  val history: History            = History.readOrGenerate(settings)
//  val pbr    : ProgramBoxRegistry = ProgramBoxRegistry.readOrGenerate(settings, history.storage.storage).get

  //val uuidStateBoxes = executionBox.stateBoxUUIDs.map(v => programBoxRegistry.getBox(v).get.asInstanceOf[StateBox])

//  val codeBoxes: Seq[ProgramId] = executionBox.codeBoxIds

  //lazy val stateBoxIds: IndexedSeq[Array[Byte]] = IndexedSeq(state.head._1.id)

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = feeBoxIdKeyPairs.map(_._1)

  lazy val hashNoNonces: Digest32 =
    FastCryptographicHash(
      executionBox.id ++
        methodName.getBytes ++
        owner.pubKeyBytes ++
        methodParams.noSpaces.getBytes ++
        Longs.toByteArray(timestamp) ++
        fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
    )

  override lazy val newBoxes: Traversable[ProgramBox] = {
    //    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
    val digest = FastCryptographicHash(proposition.pubKeyBytes ++ hashNoNonces)

    val nonce = ProgramTransaction.nonceFromDigest(digest)

    val programResult: Json =
        Program.execute(stateBoxes, codeBoxes, methodName)(owner)(methodParams.asObject.get) match {
          case Success(res) => res
          case Failure(ex)  => throw ex
        }

    // enforces that the only editable state box is the first state box
    val updatedStateBox: StateBox = StateBox(owner, nonce, stateBoxes.head.value, programResult)

    IndexedSeq(updatedStateBox)
  }

  lazy val json: Json = (
    commonJson.asObject.get.toMap ++ Map(
      "state" -> stateBoxes.map { _.json }.asJson,
      "code" -> codeBoxes.map { _.json }.asJson,
      "methodName" -> methodName.asJson,
      "methodParams" -> methodParams,
      "newBoxes" -> newBoxes.map { _.json }.toSeq.asJson
      )).asJson

  override lazy val serializer: BifrostSerializer[ProgramMethodExecution] = ProgramMethodExecutionSerializer

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    FastCryptographicHash(executionBox.bytes ++ hashNoNonces),
    data.getBytes
    )

//  def assetNonce ( prop: PublicKey25519Proposition, hashNoNonces: Array[Byte] ): Nonce = ProgramTransaction
//    .nonceFromDigest(
//      FastCryptographicHash("assetNonce".getBytes
//                              ++ prop.pubKeyBytes
//                              ++ hashNoNonces)
//      )

  override def toString: String = s"ProgramMethodExecution(${json.noSpaces})"
}

object ProgramMethodExecution {

  type SR = StateReader[Box]

  //YT NOTE - example of how to use static function to construct methodParams for PME tx

  //noinspection ScalaStyle
  def create ( state       : State,
               pbr         : ProgramBoxRegistry,
               programId   : ProgramId,
               methodName  : String,
               methodParams: Json,
               owner       : PublicKey25519Proposition,
               signatures  : Map[PublicKey25519Proposition, Signature25519],
               preFeeBoxes : Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]],
               fees        : Map[PublicKey25519Proposition, Long],
               timestamp   : Long,
               data        : String
             ): Try[ProgramMethodExecution] = Try {

    val execBox = state.getProgramBox[ExecutionBox](programId).get

    val stateBoxes: Seq[StateBox] = execBox.stateBoxIds.map(sb => state.getProgramBox[StateBox](sb).get)

    val codeBoxes: Seq[CodeBox]  = execBox.codeBoxIds.map(cb => state.getProgramBox[CodeBox](cb).get)

    ProgramMethodExecution(execBox, stateBoxes, codeBoxes, methodName, methodParams, owner, signatures, preFeeBoxes, fees, timestamp, data)
  }

  def syntacticValidate ( tx: ProgramMethodExecution, withSigs: Boolean = true ): Try[Unit] = Try {
    require(tx.signatures(tx.owner).isValid(tx.owner, tx.messageToSign)
            , "Either an invalid signature was submitted or the party listed was not part of the program.")
  }.flatMap(_ => ProgramTransaction.commonValidation(tx))

  def validatePrototype ( tx: ProgramMethodExecution ): Try[Unit] = syntacticValidate(tx, withSigs = false)

  def semanticValidate ( tx: ProgramMethodExecution, state: SR ): Try[Unit] = {
    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    /* Program exists */
    if ( state.getBox(tx.executionBox.id).isEmpty ) {
      throw new TransactionValidationException(s"Program ${Base58.encode(tx.executionBox.id)} does not exist")
    }

    //TODO get execution box from box registry using UUID before using its actual id to get it from storage
    val executionBox: ExecutionBox = state.getBox(tx.executionBox.id).get.asInstanceOf[ExecutionBox]
    val programProposition: PublicKey25519Proposition = executionBox.proposition

    /* This person belongs to program */
    if ( !MultiSignature25519(tx.signatures.values.toSet).isValid(programProposition, tx.messageToSign) ) {
      throw new TransactionValidationException(s"Signature is invalid for ExecutionBox")
    }

    // make sure we are not attempting to change an already deployed program
    if ( tx.newBoxes.forall(curBox => state.getBox(curBox.id).isDefined) ) {
       throw new TransactionValidationException("ProgramCreation attempts to overwrite existing program")
    }

    // check that the provided signatures generate valid unlockers
    val unlockers = State.generateUnlockers(tx.boxIdsToOpen, tx.signatures.head._2)
    unlockers
      .foldLeft[Try[Unit]](Success(()))(( tryUnlock, unlocker ) => {
        tryUnlock
          .flatMap { _ =>
            state.getBox(unlocker.closedBoxId) match {
              case Some(box) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) =>
                Success(Unit)

              case _ =>
                Failure(new TransactionValidationException(s"Invalid transaction"))
            }
          }
      })
  }

  implicit val decodeProgramMethodExecution: Decoder[ProgramMethodExecution] =
    ( c: HCursor ) => for {
      stateBoxes <- c.downField("state").as[Seq[StateBox]]
      codeBoxes <- c.downField("code").as[Seq[CodeBox]]
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
        executionBox,
        stateBoxes,
        codeBoxes,
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