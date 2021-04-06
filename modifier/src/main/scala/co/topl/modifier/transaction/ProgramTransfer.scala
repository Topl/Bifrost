//package co.topl.modifier.transaction
//
//import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
//import co.topl.attestation.proof.SignatureCurve25519
//import co.topl.nodeView.state.StateReader
//import co.topl.modifier.box.{ Box, BoxId, ExecutionBox, ProgramBox }
//import com.google.common.primitives.{ Bytes, Longs }
//import io.circe.syntax._
//import io.circe.{ Decoder, Encoder, HCursor }
//import scorex.crypto.hash.Blake2b256
//
//import scala.util.{ Failure, Success, Try }
//
//case class ProgramTransfer (from        : PublicKeyCurve25519Proposition,
//                            to          : PublicKeyCurve25519Proposition,
//                            signature   : SignatureCurve25519,
//                            executionBox: ExecutionBox,
//                            fee         : Long,
//                            timestamp   : Long,
//                            data        : String
//                           ) extends Transaction {
//
//  lazy val hashNoNonces: Array[Byte] = Blake2b256(
//    to.pubKeyBytes
//      ++ Longs.toByteArray(fee)
//      ++ data.getBytes
//    )
//
//  override lazy val boxIdsToOpen: IndexedSeq[BoxId] = IndexedSeq(executionBox.id)
//
//  override lazy val newBoxes: Traversable[ProgramBox] = {
//
//    val nonce = Transaction.nonceFromDigest(
//      Blake2b256("ProgramTransfer".getBytes
//                              ++ to.pubKeyBytes
//                              ++ hashNoNonces
//                            ))
//
//    Seq(ExecutionBox(to, nonce, executionBox.value, executionBox.stateBoxIds, executionBox.codeBoxIds))
//  }
//
//  override lazy val messageToSign: Array[Byte] = Bytes.concat(
//    "ProgramTransfer".getBytes
//      ++ to.pubKeyBytes
//      ++ newBoxes.head.bytes
//      ++ Longs.toByteArray(fee)
//    )
//
//  override def toString: String = s"ProgramTransfer(${json.noSpaces})"
//}
//
//object ProgramTransfer {
//
//  type SR = StateReader[Box]
//
//  implicit val jsonEncoder: Encoder[ProgramTransfer] = { tx: ProgramTransfer =>
//    Map(
//      "txHash" -> tx.id.toString.asJson,
//      "txType" -> "ProgramTransfer".asJson,
//      "newBoxes" -> tx.newBoxes.head.id.toString.asJson,
//      "boxesToRemove" -> tx.boxIdsToOpen.head.toString.asJson,
//      "from" -> tx.from.toString.asJson,
//      "to" -> tx.to.toString.asJson,
//      "signature" -> tx.signature.toString.asJson,
//      "fee" -> tx.fee.asJson,
//      "timestamp" -> tx.timestamp.asJson,
//      "data" -> tx.data.asJson
//      ).asJson
//  }
//
//  implicit val jsonDecoder: Decoder[ProgramTransfer] = (c: HCursor) =>
//    for {
//      from <- c.downField("from").as[PublicKeyCurve25519Proposition]
//      to <- c.downField("to").as[PublicKeyCurve25519Proposition]
//      signature <- c.downField("signature").as[SignatureCurve25519]
//      executionBox <- c.downField("executionBox").as[ExecutionBox]
//      fee <- c.downField("fee").as[Long]
//      timestamp <- c.downField("timestamp").as[Long]
//      data <- c.downField("data").as[String]
//    } yield {
//      ProgramTransfer(from, to, signature, executionBox, fee, timestamp, data)
//    }
//
//  //TODO implement prototype tx
//  /*def createPrototype(tokenBoxRegistry: TokenBoxRegistry,
//                      toReceive: IndexedSeq[(PublicKey25519Proposition, Long)],
//                      sender: IndexedSeq[PublicKey25519Proposition],
//                      fee: Long, data: String): Try[PolyTransfer] = Try
//  {
//    val params = parametersForCreate(tokenBoxRegistry, toReceive, sender, fee, "ProgramTransfer")
//    val timestamp = Instant.now.toEpochMilli
//    ProgramTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), fee, timestamp, data)
//  }
//   */
//
//  def validatePrototype ( tx: ProgramTransfer ): Try[Unit] = syntacticValidate(tx, withSigs = false)
//
//  def syntacticValidate ( tx: ProgramTransfer, withSigs: Boolean = true ): Try[Unit] = Try {
//    require(tx.fee >= 0)
//    require(tx.timestamp >= 0)
//
//    if ( withSigs ) {
//      require(tx.signature.isValid(tx.from, tx.messageToSign))
//    }
//
//    // only allow a single box to be transferred
//    tx.newBoxes.size match {
//      case 1 if (tx.newBoxes.head.isInstanceOf[ExecutionBox]) => Success(Unit)
//      case _                                                  => Failure(new Exception("Invalid transaction"))
//    }
//
//    // ensure unique list of inuts and output
//    require(tx.newBoxes.forall(b ⇒ !tx.boxIdsToOpen.contains(b.id)))
//  }
//
//  def semanticValidate ( tx: ProgramTransfer, state: SR ): Try[Unit] = {
//
//    // check that the transaction is correctly formed before checking state
//    syntacticValidate(tx) match {
//      case Failure(e) => throw e
//      case _          => // continue processing
//    }
//
//    val from = Seq((tx.from, tx.executionBox.nonce))
//    val signature = Map(tx.from -> tx.signature)
//    val unlocker = ProgramBox.generateUnlockers(from, signature).head
//
//    state.getBox(unlocker.closedBoxId) match {
//      case Some(box: ExecutionBox) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) => Success(Unit)
//      case Some(_) => Failure(new Exception("Invalid unlocker"))
//      case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
//      case _       => Failure(new Exception("Invalid Box type for this transaction"))
//    }
//  }
//}
//
////def createAndApply ( w           : Wallet,
////from        : PublicKey25519Proposition,
////to          : PublicKey25519Proposition,
////executionBox: ExecutionBox,
////fee         : Long,
////data        : String
////): Try[ProgramTransfer] = Try {
////
////  val selectedSecret = w.secretByPublicImage(from).get
////  val fakeSig = Signature25519(Array())
////  val timestamp = Instant.now.toEpochMilli
////  val messageToSign = ProgramTransfer(from, to, fakeSig, executionBox, fee, timestamp, data).messageToSign
////
////  val signature = selectedSecret.sign(messageToSign)
////
////  ProgramTransfer(from, to, signature, executionBox, fee, timestamp, data)
////}
