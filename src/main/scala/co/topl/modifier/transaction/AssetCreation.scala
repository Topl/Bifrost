//package co.topl.modifier.transaction
//
//import java.time.Instant
//
//import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
//import co.topl.attestation.proof.SignatureCurve25519
//import co.topl.nodeView.state.StateReader
//import co.topl.nodeView.state.box.{ AssetBox, Box, BoxId, TokenBox }
//import com.google.common.primitives.{ Bytes, Ints, Longs }
//import io.circe.syntax._
//import io.circe.{ Decoder, Encoder, HCursor }
//import scorex.crypto.hash.Blake2b256
//
//import scala.util.{ Failure, Success, Try }
//
//case class AssetCreation (to: IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
//                          signatures: Map[PublicKeyCurve25519Proposition, SignatureCurve25519],
//                          assetCode : String,
//                          issuer    : PublicKeyCurve25519Proposition,
//                          fee       : Long,
//                          timestamp : Long,
//                          data      : String
//                         ) extends Transaction {
//
//  override lazy val boxIdsToOpen: IndexedSeq[BoxId] = IndexedSeq()
//
//  //TODO deprecate timestamp once fee boxes are included in nonce generation
//  lazy val hashNoNonces: Array[Byte] = Blake2b256(
//    to.flatMap(_._1.pubKeyBytes).toArray ++
//    Longs.toByteArray(timestamp) ++
//    Longs.toByteArray(fee)
//  )
//
//  override lazy val newBoxes: Traversable[TokenBox] = {
//    to
//      .filter(toInstance => toInstance._2 > 0L)
//      .zipWithIndex
//      .map { case ((prop, value), idx) =>
//        val nonce = Transaction.nonceFromDigest(
//          Blake2b256(
//            "AssetCreation".getBytes ++
//              prop.pubKeyBytes ++
//              issuer.pubKeyBytes ++
//              assetCode.getBytes ++
//              hashNoNonces ++
//              Ints.toByteArray(idx)
//            )
//          )
//        AssetBox(prop, nonce, value, assetCode, issuer, data)
//      }
//  }
//
//  override lazy val messageToSign: Array[Byte] = Bytes.concat(
//    "AssetCreation".getBytes(),
//    to.flatMap(_._1.pubKeyBytes).toArray ++
//      newBoxes.foldLeft(Array[Byte]())(( acc, x ) => acc ++ x.bytes),
//    issuer.pubKeyBytes,
//    assetCode.getBytes,
//    Longs.toByteArray(fee),
//    data.getBytes
//    )
//
//  override def toString: String = s"AssetCreation(${json.noSpaces})"
//
//}
//
//object AssetCreation {
//
//  type SR = StateReader[Box]
//
//  implicit val jsonEncoder: Encoder[AssetCreation] = { tx: AssetCreation =>
//    Map(
//      "txHash" -> tx.id.toString.asJson,
//      "txType" -> "AssetCreation".asJson,
//      "newBoxes" -> tx.newBoxes.map(b => b.id.toString).toSeq.asJson,
//      "to" -> tx.to.asJson,
//      "issuer" -> tx.issuer.asJson,
//      "assetCode" -> tx.assetCode.asJson,
//      "signatures" -> tx.signatures.asJson,
//      "fee" -> tx.fee.asJson,
//      "timestamp" -> tx.timestamp.asJson,
//      "data" -> tx.data.asJson
//      ).asJson
//  }
//
//  implicit val jsonDecoder: Decoder[AssetCreation] = ( c: HCursor ) =>
//    for {
//      to <- c.downField("to").as[IndexedSeq[(PublicKeyCurve25519Proposition, Long)]]
//      signatures <- c.downField("signatures").as[Map[PublicKeyCurve25519Proposition, SignatureCurve25519]]
//      assetCode <- c.downField("assetCode").as[String]
//      issuer <- c.downField("issuer").as[PublicKeyCurve25519Proposition]
//      fee <- c.downField("fee").as[Long]
//      timestamp <- c.downField("timestamp").as[Long]
//      data <- c.downField("data").as[String]
//    } yield {
//      AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
//    }
//
//  def createRaw (to: IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
//                 fee: Long,
//                 issuer: PublicKeyCurve25519Proposition,
//                 assetCode: String,
//                 data: String
//                ): Try[AssetCreation] = Try {
//
//    val timestamp = Instant.now.toEpochMilli
//
//    AssetCreation(to, Map(), assetCode, issuer, fee, timestamp, data)
//  }
//
//  def syntacticValidate ( tx: AssetCreation, withSigs: Boolean = true ): Try[Unit] = Try {
//    require(tx.to.forall(_._2 > 0L))
//    require(tx.fee >= 0)
//    require(tx.timestamp >= 0)
//
//    if ( withSigs ) {
//      require(tx.signatures.forall({ case (prop, signature) =>
//        signature.isValid(prop, tx.messageToSign)
//      }),
//              "Invalid signatures"
//              )
//    }
//
//    tx.newBoxes.size match {
//      //only one box should be created
//      case 1 if (tx.newBoxes.head.isInstanceOf[AssetBox]) => Success(Unit)
//      case _                                              => Failure(new Exception("Invalid transaction"))
//    }
//  }
//
//  def validatePrototype ( tx: AssetCreation ): Try[Unit] = syntacticValidate(tx, withSigs = false)
//
//  def semanticValidate ( tx: AssetCreation, state: SR ): Try[Unit] = {
//
//    // check that the transaction is correctly formed before checking state
//    syntacticValidate(tx)
//
//  }
//}
//
///**
// * Route here from AssetApiRoute
// * Assumes that the WalletTrait contains the issuer's key information
// * Takes WalletTrait from current view, and generates signature from issuer's public key
// * Forms corresponding AssetCreation transaction
// */
////  def createAndApply (w        : Wallet,
////                      to       : IndexedSeq[(PublicKey25519Proposition, Long)],
////                      fee      : Long,
////                      issuer   : PublicKey25519Proposition,
////                      assetCode: String,
////                      data     : String
////                     ): Try[AssetCreation] = Try {
////
////    val selectedSecret = w.secretByPublicImage(issuer).get
////    val timestamp = Instant.now.toEpochMilli
////    val messageToSign = AssetCreation(to, Map(), assetCode, issuer, fee, timestamp, data).messageToSign
////
////    val signatures = Map(issuer -> selectedSecret.sign(messageToSign))
////
////    AssetCreation(to, signatures, assetCode, issuer, fee, timestamp, data)
////  }
