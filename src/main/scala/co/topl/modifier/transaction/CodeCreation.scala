//package co.topl.modifier.transaction
//
//import co.topl.attestation.Address
//import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
//import co.topl.attestation.proof.SignatureCurve25519
//import co.topl.nodeView.state.box.{ Box, BoxId, CodeBox, ProgramBox }
//import co.topl.nodeView.state.StateReader
//import co.topl.program.ProgramPreprocessor
//import com.google.common.primitives.{ Bytes, Longs }
//import io.circe.syntax._
//import io.circe.{ Decoder, Encoder, HCursor, JsonObject }
//import scorex.crypto.hash.Blake2b256
//
//import scala.util.{ Failure, Success, Try }
//
//case class CodeCreation (to       : Address,
//                         signature: SignatureCurve25519,
//                         code     : String,
//                         fee      : Long,
//                         timestamp: Long,
//                         data     : String
//                        ) extends Transaction {
//
//  override lazy val boxIdsToOpen: IndexedSeq[BoxId] = IndexedSeq()
//
//  lazy val hashNoNonces: Array[Byte] = Blake2b256(
//    to.pubKeyBytes ++
//      code.getBytes ++
//      Longs.toByteArray(fee) ++
//      Longs.toByteArray(timestamp)
//    )
//
//  override val newBoxes: Traversable[Box] = {
//
//    val nonceGen = Blake2b256(
//      "CodeCreation".getBytes ++
//        to.pubKeyBytes ++
//        code.getBytes ++
//        hashNoNonces
//      )
//
//    val nonce = Transaction.nonceFromDigest(nonceGen)
//
//    val programId = ProgramId.create(nonceGen ++ "programId".getBytes)
//
//    val interface = ProgramPreprocessor("code", code)(JsonObject.empty).interface
//
//    Seq(CodeBox(to, nonce, programId, Seq(code), interface))
//  }
//
//  override lazy val messageToSign: Array[Byte] = Bytes.concat(
//    "CodeCreation".getBytes,
//    to.pubKeyBytes,
//    newBoxes.foldLeft(Array[Byte]())(( a, b ) => a ++ b.bytes),
//    code.getBytes,
//    Longs.toByteArray(fee),
//    data.getBytes
//    )
//
//  override def toString: String = s"CodeCreation(${json.noSpaces})"
//}
//
//
//
//
//object CodeCreation {
//
//  type SR = StateReader[ProgramBox]
//
//  implicit val jsonEncoder: Encoder[CodeCreation] = ( tx: CodeCreation ) =>
//    Map(
//      "txHash" -> tx.id.asJson,
//      "txType" -> "CodeCreation".asJson,
//      "newBoxes" -> tx.newBoxes.map(b => b.id.toString.asJson).toSeq.asJson,
//      "to" -> tx.to.asJson,
//      "signature" -> tx.signature.asJson,
//      "code" -> tx.code.asJson,
//      "fee" -> tx.fee.asJson,
//      "timestamp" -> tx.timestamp.asJson,
//      "data" -> tx.data.asJson
//      ).asJson
//
//  implicit val jsonDecoder: Decoder[CodeCreation] = (c: HCursor) =>
//    for {
//      to <- c.downField("to").as[PublicKeyCurve25519Proposition]
//      signature <- c.downField("signature").as[SignatureCurve25519]
//      code <- c.downField("code").as[String]
//      fee <- c.downField("fee").as[Long]
//      timestamp <-c.downField("timestamp").as[Long]
//      data <- c.downField("data").as[String]
//    } yield {
//      CodeCreation(to, signature, code, fee, timestamp, data)
//    }
//
//  /**
//   *
//   * @param tx
//   * @param withSigs
//   * @return
//   */
//  def syntacticValidate ( tx: CodeCreation, withSigs: Boolean = true ): Try[Unit] = Try {
//    require(tx.fee >= 0)
//    require(tx.timestamp >= 0)
//    require(tx.signature.isValid(tx.to, tx.messageToSign), "Invalid signature")
//
//    tx.newBoxes.size match {
//      //only one box should be created
//      case 1 if (tx.newBoxes.head.isInstanceOf[CodeBox]) => Success(Unit)
//      case _                                             => Failure(new Exception("Invlid transaction"))
//    }
//  }
//
//  /**
//   *
//   * @param tx
//   * @return
//   */
//  def validatePrototype ( tx: CodeCreation ): Try[Unit] = syntacticValidate(tx, withSigs = false)
//
//  /**
//   * Check the code is valid chain code and the newly created CodeBox is
//   * formed properly
//   *
//   * @param tx : CodeCreation transaction
//   * @return
//   */
//  def semanticValidate ( tx: CodeCreation, state: SR ): Try[Unit] = {
//
//    // check that the transaction is correctly formed before checking state
//    syntacticValidate(tx)
//
//  }
//}
