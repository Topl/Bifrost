//accumulators co.topl.modifier.transaction
//
//import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
//import co.topl.attestation.proof.SignatureCurve25519
//import co.topl.modifier.transaction.Transaction.Nonce
//import co.topl.modifier.box.{ Box, _ }
//import co.topl.nodeView.state.StateReader
//import co.topl.program.{ ExecutionBuilder, ExecutionBuilderSerializer }
//import co.topl.utils.exceptions.TransactionValidationException
//import com.google.common.primitives.{ Bytes, Ints, Longs }
//import io.circe.syntax._
//import io.circe.{ Decoder, Encoder, HCursor }
//import co.topl.crypto.hash.Blake2b256
//
//import scala.util.{ Failure, Success, Try }
//
///**
//  *
//  * @param executionBuilder   the ExecutionBuilder object containing the terms for the proposed program
//  * @param readOnlyStateBoxes a list of StateBoxes to be used in evaluating the program, but never mutated
//  *                           beyond the context of the evaluation
//  * @param preInvestmentBoxes a list of box nonces corresponding to the PolyBoxes to be used to fund the investment
//  * @param owner              the public key used to sign and create newboxes
//  * @param signatures         a mapping specifying the signatures by each public key for this transaction
//  * @param preFeeBoxes        a mapping specifying box nonces and amounts corresponding to the PolyBoxes to be used to
//  *                           pay fees for each party contributing fees
//  * @param fees               a mapping specifying the amount each party is contributing to the fees
//  * @param timestamp          the timestamp of this transaction
//  */
//case class ProgramCreation(executionBuilder: ExecutionBuilder,
//                           readOnlyStateBoxes: Seq[ProgramId],
//                           preInvestmentBoxes: IndexedSeq[(Nonce, Long)],
//                           owner: PublicKeyCurve25519Proposition,
//                           signatures: Map[PublicKeyCurve25519Proposition, SignatureCurve25519],
//                           preFeeBoxes: Map[PublicKeyCurve25519Proposition, IndexedSeq[(Nonce, Long)]],
//                           fees: Map[PublicKeyCurve25519Proposition, Long],
//                           timestamp: Long,
//                           data: String)
//  extends ProgramTransaction {
//
////  lazy val proposition = MofNProposition(1, parties.map(_._1.pubKeyBytes).toSet)
//
//  lazy val investmentBoxIds: IndexedSeq[BoxId] =
//    preInvestmentBoxes.map(n => {
//      PublicKeyNoncedBox.idFromBox(owner, n._1)})
//
//  lazy val boxIdsToOpen: IndexedSeq[BoxId] = IndexedSeq() //investmentBoxIds ++ feeBoxIdKeyPairs.map(_._1)
//
//  lazy val hashNoNonces: Array[Byte] = Blake2b256(
//    ExecutionBuilderSerializer.toBytes(executionBuilder) ++
//      owner.pubKeyBytes ++
//      //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _) ++
//      fees.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)))
//
//  lazy val newStateBoxes: IndexedSeq[StateBox] = {
//    val nonceGen = Blake2b256("stateBox".getBytes
//                                           ++ executionBuilder.core.variables.noSpaces.getBytes
//                                           ++ hashNoNonces
//                                           ++ Ints.toByteArray(0))
//
//    val nonce = Transaction.nonceFromDigest(nonceGen)
//
//    val stateBox = StateBox(owner, nonce, ProgramId.create(nonceGen ++ "programId".getBytes), executionBuilder.core.variables)
//
//    IndexedSeq(stateBox)
//  }
//
//  override lazy val newBoxes: Traversable[ProgramBox] = {
//
//    val availableBoxes: Set[(Nonce, Long)] = (preFeeBoxes(owner) ++ preInvestmentBoxes).toSet
//    val canSend = availableBoxes.map(_._2).sum
//    val leftOver: Long = canSend - fees(owner)
//
//    // generate nonces for the boxes and program ids for the new program
//    val investorNonce = Transaction.nonceFromDigest(
//      Blake2b256("ProgramCreation".getBytes
//        ++ owner.pubKeyBytes
//        ++ hashNoNonces
//        ++ Ints.toByteArray(0))
//    )
//
//    val cbNonceGen = Blake2b256("codeBox".getBytes
//                                             ++ executionBuilder.core.code.values.foldLeft(Array[Byte]())((a,b) => a ++ b.getBytes())
//                                             ++ hashNoNonces
//                                             ++ Ints.toByteArray(0))
//    val codeNonce = Transaction.nonceFromDigest(cbNonceGen)
//    val cbProgramId = ProgramId.create(cbNonceGen ++ "programId".getBytes)
//
//    val execNonceGen = Blake2b256("executionBuilder".getBytes
//                                              ++ hashNoNonces
//                                              ++ Ints.toByteArray(0))
//    val execNonce = Transaction.nonceFromDigest(execNonceGen)
//    val execProgramId = ProgramId.create(execNonceGen ++ "programId".getBytes)
//
//    // create the new boxes
//    val codeBox = CodeBox(owner, codeNonce, cbProgramId, executionBuilder.core.code.values.toSeq, executionBuilder.core.interface)
//
//    val stateBoxIds: Seq[ProgramId] = newStateBoxes.map(_.value) ++ readOnlyStateBoxes
//
//    val executionBox = ExecutionBox(owner, execNonce, execProgramId, stateBoxIds, Seq(codeBox.value))
//
//    //val investorDeductedBox: PolyBox = PolyBox(owner, investorNonce, leftOver)
//
//    IndexedSeq(executionBox, codeBox) ++ newStateBoxes //:+ investorDeductedBox // nonInvestorDeductedBoxes
//  }
//
//  override lazy val messageToSign: Array[Byte] = Bytes.concat(
//    ExecutionBuilderSerializer.toBytes(executionBuilder),
//    owner.pubKeyBytes,
//    data.getBytes
//    //boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _)
//  )
//
//  override def toString: String = s"ProgramCreation(${json.noSpaces})"
//}
//
//
//
//
//object ProgramCreation {
//
//  type SR = StateReader[Box]
//
//  implicit val jsonEncoder: Encoder[ProgramCreation] = (tx: ProgramCreation) =>
//    Map(
//      "txHash" -> tx.id.asJson,
//      "txType" -> "ProgramCreation".asJson,
//      "owner" -> tx.owner.asJson,
//      "signatures" -> tx.signatures.asJson,
//      "feePreBoxes" -> tx.preFeeBoxes.asJson,
//      "fees" -> tx.fees.asJson,
//      "timestamp" -> tx.timestamp.asJson,
//      "preInvestmentBoxes" -> tx.preInvestmentBoxes.asJson,
//      "executionBuilder" -> tx.executionBuilder.json,
//      "newBoxes" -> tx.newBoxes.map(_.json).toSeq.asJson,
//      "data" -> tx.data.asJson
//      ).asJson
//
//  implicit val jsonDecoder: Decoder[ProgramCreation] = (c: HCursor) => for {
//    executionBuilder <- c.downField("executionBuilder").as[ExecutionBuilder]
//    readOnlyStateBoxes <- c.downField("readOnlyStateBoxes").as[Seq[ProgramId]]
//    preInvestmentBoxes <- c.downField("preInvestmentBoxes").as[IndexedSeq[(Nonce, Long)]]
//    owner <- c.downField("owner").as[PublicKeyCurve25519Proposition]
//    signatures <- c.downField("signatures").as[Map[PublicKeyCurve25519Proposition, SignatureCurve25519]]
//    preFeeBoxes <- c.downField("preFeeBoxes").as[Map[PublicKeyCurve25519Proposition, IndexedSeq[(Long, Long)]]]
//    fees <- c.downField("fees").as[Map[PublicKeyCurve25519Proposition, Long]]
//    timestamp <- c.downField("timestamp").as[Long]
//    data <- c.downField("data").as[String]
//  } yield {
//    ProgramCreation(executionBuilder, readOnlyStateBoxes, preInvestmentBoxes, owner, signatures, preFeeBoxes, fees, timestamp, data)
//  }
//
//  /**
//   *
//   * @param tx
//   * @param withSigs
//   * @return
//   */
//  def syntacticValidate(tx: ProgramCreation, withSigs: Boolean = true): Try[Unit] = Try {
//    require(ExecutionBuilder.validate(tx.executionBuilder).isSuccess)
//    require(tx.signatures(tx.owner).isValid(tx.owner, tx.messageToSign), "Not all signatures were valid")
//  }.flatMap(_ => ProgramTransaction.syntacticValidate(tx))
//
//  /**
//   *
//   * @param tx
//   * @return
//   */
//  def validatePrototype(tx: ProgramCreation): Try[Unit] = syntacticValidate(tx, withSigs = false)
//
//  /**
//    * Validates ProgramCreation instance on its unlockers && timestamp of the program
//    *
//    * @param tx : ProgramCreation transaction
//    * @return
//    */
//  def semanticValidate(tx: ProgramCreation, state: SR): Try[Unit] = {
//    Try{
//
//      // check that the transaction is correctly formed before checking state
//      syntacticValidate(tx) match {
//        case Failure(e) => throw e
//        case _ => // continue processing
//      }
//
//      // make sure we are not attempting to change an already deployed program
//      if (tx.newBoxes.forall(curBox => state.getBox(curBox.id).isDefined)) {
//        throw new TransactionValidationException("ProgramCreation attempts to overwrite existing program")
//      }
//
//      // validate the unlockers generated by the signatures
//      val unlockers = ProgramBox.generateUnlockers(tx.boxIdsToOpen, tx.signatures.head._2)
//
//      unlockers
//        .foldLeft[Try[Unit]](Success(Unit))((tryUnlock, unlocker) => {
//          tryUnlock
//            .flatMap { _ =>
//              state.getBox(unlocker.closedBoxId) match {
//                case Some(box) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) => Success(Unit)
//                case _ => throw new TransactionValidationException(s"Box for unlocker $unlocker is not in the state")
//              }
//            }
//        })
//    }
//  }
//}