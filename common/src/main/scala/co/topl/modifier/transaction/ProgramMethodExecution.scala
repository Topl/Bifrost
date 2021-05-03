//package co.topl.modifier.transaction
//
//import co.topl.attestation.proof.{ SignatureCurve25519, ThresholdSignatureCurve25519 }
//import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
//import co.topl.modifier.box._
//import co.topl.nodeView.state.{ State, StateReader }
//import co.topl.program.Program
//import co.topl.utils.exceptions.TransactionValidationException
//import com.google.common.primitives.{ Bytes, Longs }
//import io.circe.syntax._
//import io.circe.{ Decoder, Encoder, HCursor, Json }
//import scorex.crypto.hash.{ Blake2b256, Digest32 }
//
//import scala.util.{ Failure, Success, Try }
//
//case class ProgramMethodExecution (executionBox: ExecutionBox,
//                                   stateBoxes  : Seq[StateBox],
//                                   codeBoxes   : Seq[CodeBox],
//                                   methodName  : String,
//                                   methodParams: Json,
//                                   owner       : PublicKeyCurve25519Proposition,
//                                   signatures  : Map[PublicKeyCurve25519Proposition, SignatureCurve25519],
//                                   preFeeBoxes : Map[PublicKeyCurve25519Proposition, IndexedSeq[(Nonce, Long)]],
//                                   fees        : Map[PublicKeyCurve25519Proposition, Long],
//                                   timestamp   : Long,
//                                   data        : String
//                                  ) extends ProgramTransaction {
//
//  val proposition: PublicKeyCurve25519Proposition = executionBox.proposition
//
//  // TODO Fix instantiation to handle runtime input and/or extract to a better location
////  val config  : Config      = ConfigFactory.load("application")
////  val settings: AppSettings = AppSettings.fromConfig(config)
//
//  //TODO do not readOrGenerate programBoxRegistry here
//  //ProgramBoxRegistry should be taken from nodeView at api level and passed as parameter to static function in companion object
//  //Static function should extract necessary boxes and use those as methodParams to transaction class
//  //See static create function in companion object below
//
////  val history: History            = History.readOrGenerate(settings)
////  val pbr    : ProgramBoxRegistry = ProgramBoxRegistry.readOrGenerate(settings, history.storage.storage).get
//
//  //val uuidStateBoxes = executionBox.stateBoxUUIDs.map(v => programBoxRegistry.getBox(v).get.asInstanceOf[StateBox])
//
////  val codeBoxes: Seq[ProgramId] = executionBox.codeBoxIds
//
//  //lazy val stateBoxIds: IndexedSeq[Array[Byte]] = IndexedSeq(state.head._1.id)
//
//  lazy val boxIdsToOpen: IndexedSeq[BoxId] = feeBoxIdKeyPairs.map(_._1)
//
//  lazy val hashNoNonces: Digest32 =
//    Blake2b256(
//      executionBox.id.hashBytes ++
//        methodName.getBytes ++
//        owner.pubKeyBytes ++
//        methodParams.noSpaces.getBytes ++
//        Longs.toByteArray(timestamp) ++
//        fees.flatMap { case (prop, value) => prop.pubKeyBytes ++ Longs.toByteArray(value) }
//    )
//
//  override lazy val newBoxes: Traversable[ProgramBox] = {
//    //    val digest = FastCryptographicHash(MofNPropositionSerializer.toBytes(proposition) ++ hashNoNonces)
//    val digest = Blake2b256(proposition.pubKeyBytes ++ hashNoNonces)
//
//    val nonce = Transaction.nonceFromDigest(digest)
//
//    val programResult: Json =
//        Program.execute(stateBoxes, codeBoxes, methodName)(owner)(methodParams.asObject.get) match {
//          case Success(res) => res
//          case Failure(ex)  => throw ex
//        }
//
//    // enforces that the only editable state box is the first state box
//    val updatedStateBox: StateBox = StateBox(owner, nonce, stateBoxes.head.value, programResult)
//
//    IndexedSeq(updatedStateBox)
//  }
//
//  override lazy val messageToSign: Array[Byte] = Bytes.concat(
//    Blake2b256(executionBox.bytes ++ hashNoNonces),
//    data.getBytes
//    )
//
////  def assetNonce ( prop: PublicKey25519Proposition, hashNoNonces: Array[Byte] ): Nonce = ProgramTransaction
////    .nonceFromDigest(
////      FastCryptographicHash("assetNonce".getBytes
////                              ++ prop.pubKeyBytes
////                              ++ hashNoNonces)
////      )
//
//  override def toString: String = s"ProgramMethodExecution(${json.noSpaces})"
//
//}
//
//
//
//
//
//object ProgramMethodExecution {
//
//  type SR = StateReader[Box]
//
//  implicit val jsonEncoder: Encoder[ProgramMethodExecution] = (tx: ProgramMethodExecution) =>
//    Map(
//      "txHash" -> tx.id.asJson,
//      "txType" -> "ProgramMethodExecution".asJson,
//      "owner" -> tx.owner.asJson,
//      "signatures" -> tx.signatures.asJson,
//      "feePreBoxes" -> tx.preFeeBoxes.asJson,
//      "fees" -> tx.fees.asJson,
//      "timestamp" -> tx.timestamp.asJson,
//      "state" -> tx.stateBoxes.asJson,
//      "code" -> tx.codeBoxes.asJson,
//      "methodName" -> tx.methodName.asJson,
//      "methodParams" -> tx.methodParams,
//      "newBoxes" -> tx.newBoxes.map { _.json }.toSeq.asJson
//      ).asJson
//
//  implicit val jsonDecoder: Decoder[ProgramMethodExecution] = ( c: HCursor ) =>
//    for {
//      executionBox <- c.downField("executionBox").as[ExecutionBox]
//      stateBoxes <- c.downField("state").as[Seq[StateBox]]
//      codeBoxes <- c.downField("code").as[Seq[CodeBox]]
//      methodName <- c.downField("methodName").as[String]
//      methodParams <- c.downField("methodParams").as[Json]
//      owner <- c.downField("owner").as[PublicKeyCurve25519Proposition]
//      signatures <- c.downField("signatures").as[Map[PublicKeyCurve25519Proposition, SignatureCurve25519]]
//      preFeeBoxes <- c.downField("preFeeBoxes").as[Map[PublicKeyCurve25519Proposition, IndexedSeq[(Long, Long)]]]
//      fees <- c.downField("fees").as[Map[PublicKeyCurve25519Proposition, Long]]
//      timestamp <- c.downField("timestamp").as[Long]
//      data <- c.downField("data").as[String]
//    } yield {
//      ProgramMethodExecution(executionBox, stateBoxes, codeBoxes, methodName, methodParams,
//                             owner, signatures, preFeeBoxes, fees, timestamp, data)
//    }
//
//  /**
//   *
//   * @param state
//   * @param programId
//   * @param methodName
//   * @param methodParams
//   * @param owner
//   * @param signatures
//   * @param preFeeBoxes
//   * @param fees
//   * @param timestamp
//   * @param data
//   * @return
//   */
//  def create (state       : State,
//              programId   : ProgramId,
//              methodName  : String,
//              methodParams: Json,
//              owner       : PublicKeyCurve25519Proposition,
//              signatures  : Map[PublicKeyCurve25519Proposition, SignatureCurve25519],
//              preFeeBoxes : Map[PublicKeyCurve25519Proposition, IndexedSeq[(Nonce, Long)]],
//              fees        : Map[PublicKeyCurve25519Proposition, Long],
//              timestamp   : Long,
//              data        : String
//             ): Try[ProgramMethodExecution] = Try {
//
//    val execBox = state.getProgramBox[ExecutionBox](programId).get
//
//    val stateBoxes: Seq[StateBox] = execBox.stateBoxIds.map(sb => state.getProgramBox[StateBox](sb).get)
//
//    val codeBoxes: Seq[CodeBox]  = execBox.codeBoxIds.map(cb => state.getProgramBox[CodeBox](cb).get)
//
//    ProgramMethodExecution(execBox, stateBoxes, codeBoxes, methodName, methodParams, owner, signatures, preFeeBoxes, fees, timestamp, data)
//  }
//
//  /**
//   *
//   * @param tx
//   * @param withSigs
//   * @return
//   */
//  def syntacticValidate ( tx: ProgramMethodExecution, withSigs: Boolean = true ): Try[Unit] = Try {
//    require(tx.signatures(tx.owner).isValid(tx.owner, tx.messageToSign)
//            , "Either an invalid signature was submitted or the party listed was not part of the program.")
//  }.flatMap(_ => ProgramTransaction.syntacticValidate(tx))
//
//  /**
//   *
//   * @param tx
//   * @return
//   */
//  def validatePrototype ( tx: ProgramMethodExecution ): Try[Unit] = syntacticValidate(tx, withSigs = false)
//
//  /**
//   *
//   * @param tx
//   * @param state
//   * @return
//   */
//  def semanticValidate ( tx: ProgramMethodExecution, state: SR ): Try[Unit] = {
//    // check that the transaction is correctly formed before checking state
//    syntacticValidate(tx) match {
//      case Failure(e) => throw e
//      case _          => // continue processing
//    }
//
//    /* Program exists */
//    if ( state.getBox(tx.executionBox.id).isEmpty ) {
//      throw new TransactionValidationException(s"Program ${tx.executionBox.id} does not exist")
//    }
//
//    //TODO get execution box from box registry using UUID before using its actual id to get it from storage
//    val executionBox: ExecutionBox = state.getBox(tx.executionBox.id).get.asInstanceOf[ExecutionBox]
//    val programProposition: PublicKeyCurve25519Proposition = executionBox.proposition
//
//    /* This person belongs to program */
//    if ( !ThresholdSignatureCurve25519(tx.signatures.values.toSet).isValid(programProposition, tx.messageToSign) ) {
//      throw new TransactionValidationException(s"Signature is invalid for ExecutionBox")
//    }
//
//    // make sure we are not attempting to change an already deployed program
//    if ( tx.newBoxes.forall(curBox => state.getBox(curBox.id).isDefined) ) {
//       throw new TransactionValidationException("ProgramCreation attempts to overwrite existing program")
//    }
//
//    // check that the provided signatures generate valid unlockers
//    val unlockers = ProgramBox.generateUnlockers(tx.boxIdsToOpen, tx.signatures.head._2)
//    unlockers
//      .foldLeft[Try[Unit]](Success(()))(( tryUnlock, unlocker ) => {
//        tryUnlock
//          .flatMap { _ =>
//            state.getBox(unlocker.closedBoxId) match {
//              case Some(box) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) =>
//                Success(Unit)
//
//              case _ =>
//                Failure(new TransactionValidationException(s"Invalid transaction"))
//            }
//          }
//      })
//  }
//}
