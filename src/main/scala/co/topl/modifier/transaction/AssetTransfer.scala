package co.topl.modifier.transaction

import java.time.Instant

import co.topl.crypto.{ FastCryptographicHash, PrivateKey25519, Signature25519 }
import co.topl.modifier.transaction
import co.topl.modifier.transaction.Transaction.{ Nonce, Value }
import co.topl.modifier.transaction.serialization.AssetTransferSerializer
import co.topl.nodeView.state.box.AssetBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.{ State, TokenBoxRegistry }
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.{ Bytes, Ints }
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import scorex.crypto.encode.Base58

import scala.util.{ Failure, Success, Try }

case class AssetTransfer ( override val from      : IndexedSeq[(PublicKey25519Proposition, Nonce)],
                           override val to        : IndexedSeq[(PublicKey25519Proposition, Long)],
                           override val signatures: Map[PublicKey25519Proposition, Signature25519],
                           issuer                 : PublicKey25519Proposition,
                           assetCode              : String,
                           override val fee       : Long,
                           override val timestamp : Long,
                           override val data      : String
                         )
  extends TransferTransaction(from, to, signatures, fee, timestamp, data) {

  override type M = AssetTransfer

  override lazy val serializer: BifrostSerializer[AssetTransfer] = AssetTransferSerializer

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetTransfer".getBytes(),
    super.commonMessageToSign,
    issuer.pubKeyBytes,
    assetCode.getBytes,
    )

  override lazy val newBoxes: Traversable[AssetBox] = to
    .filter(toInstance => toInstance._2 > 0L)
    .zipWithIndex
    .map {
      case ((prop, value), idx) =>
        val nonce = AssetTransfer.nonceFromDigest(FastCryptographicHash(
          "AssetTransfer".getBytes ++
            prop.pubKeyBytes ++
            issuer.pubKeyBytes ++
            assetCode.getBytes ++
            hashNoNonces ++
            Ints.toByteArray(idx)
          ))
        AssetBox(prop, nonce, value, assetCode, issuer, data)
    }

  override def toJson: Json = "tmp".asJson
  override def toString: String = s"AssetTransfer(${AssetTransfer.jsonEncoder(this).noSpaces})"

}

object AssetTransfer extends TransferUtil {

  def apply ( from     : IndexedSeq[(PrivateKey25519, Nonce)],
              to       : IndexedSeq[(PublicKey25519Proposition, Value)],
              issuer   : PublicKey25519Proposition,
              assetCode: String,
              fee      : Long,
              timestamp: Long,
              data     : String
            ): AssetTransfer = {
    val params = parametersForApply(from, to, fee, timestamp, "AssetTransfer", issuer, assetCode, data).get
    transaction.AssetTransfer(params._1, to, params._2, issuer, assetCode, fee, timestamp, data)
  }

//  def create ( tbr        : TokenBoxRegistry,
//               stateReader: SR,
//               toReceive  : IndexedSeq[(PublicKey25519Proposition, Long)],
//               sender     : IndexedSeq[PublicKey25519Proposition],
//               fee        : Long,
//               issuer     : PublicKey25519Proposition,
//               assetCode  : String,
//               data       : String,
//               assetId    : Option[String] = None
//             ): Try[AssetTransfer] = Try {
//
//    val params = parametersForCreate(tbr, stateReader, w, toReceive, sender, fee, "AssetTransfer", issuer, assetCode, assetId)
//    val timestamp = Instant.now.toEpochMilli
//    AssetTransfer(params._1.map(t => t._1 -> t._2), params._2, issuer, assetCode, fee, timestamp, data)
//  }

  def createRaw (stateReader: SR,
                 toReceive  : IndexedSeq[(PublicKey25519Proposition, Long)],
                 sender     : IndexedSeq[PublicKey25519Proposition],
                 issuer     : PublicKey25519Proposition,
                 assetCode  : String,
                 fee        : Long,
                 data       : String,
                 assetId    : Option[String] = None
                 ): Try[AssetTransfer] = Try {
    val params = parametersForCreate(tbr, stateReader, toReceive, sender, fee, "AssetTransfer", issuer, assetCode, assetId)
    val timestamp = Instant.now.toEpochMilli
    AssetTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), issuer, assetCode, fee, timestamp, data)
  }

  def validatePrototype ( tx: AssetTransfer ): Try[Unit] = validateTransfer(tx, withSigs = false)

  def syntacticValidate ( tx: AssetTransfer ): Try[Unit] = validateTransfer(tx)

  def semanticValidate ( tx: AssetTransfer, state: SR ): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value).sum
    val unlockers = State.generateUnlockers(tx.from, tx.signatures)

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    unlockers.foldLeft[Try[Long]](Success(0L))(( trySum, unlocker ) => {
      trySum.flatMap { partialSum =>
        state.getBox(unlocker.closedBoxId) match {
          case Some(box: AssetBox) if unlocker.boxKey.isValid(box.proposition, tx.messageToSign) => Success(partialSum + box.value)
          case Some(_) => Failure(new Exception("Invalid unlocker"))
          case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
          case _       => Failure(new Exception("Invalid Box type for this transaction"))
        }
      }
    }) match {
      case Success(sum: Long) if txOutput == sum - tx.fee => Success(Unit)
      case Success(sum: Long) => Failure(new Exception(s"Tx output value not equal to input value. $txOutput != ${sum - tx.fee}"))
      case Failure(e)         => throw e
    }
  }

  implicit val jsonEncoder: Encoder[AssetTransfer] = { tx: AssetTransfer =>
    Map(
      "txHash" -> tx.id.toString.asJson,
      "txType" -> "AssetTransfer".asJson,
      "newBoxes" -> tx.newBoxes.map(b => Base58.encode(b.id).asJson).toSeq.asJson,
      "boxesToRemove" -> tx.boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
      "from" -> tx.from.map { s => Base58.encode(s._1.pubKeyBytes) -> s._2.toString.asJson }.asJson,
      "to" -> tx.to.map { s => Base58.encode(s._1.pubKeyBytes) -> s._2.toString.asJson }.asJson,
      "issuer" -> Base58.encode(tx.issuer.pubKeyBytes).asJson,
      "assetCode" -> tx.assetCode.asJson,
      "signatures" -> tx.signatures.map { s => Base58.encode(s._1.pubKeyBytes) -> Base58.encode(s._2.signature) }.asJson,
      "fee" -> tx.fee.asJson,
      "timestamp" -> tx.timestamp.asJson,
      "data" -> tx.data.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetTransfer] = ( c: HCursor ) =>
    for {
      rawFrom <- c.downField("from").as[IndexedSeq[(String, String)]]
      rawTo <- c.downField("to").as[IndexedSeq[(String, String)]]
      rawSignatures <- c.downField("signatures").as[Map[String, String]]
      rawIssuer <- c.downField("issuer").as[String]
      assetCode <- c.downField("assetCode").as[String]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
    } yield {
      val from = rawFrom.map { case (prop, nonce) => Transaction.stringToPubKey(prop) -> nonce.toLong }
      val to = rawTo.map { case (prop, value) => Transaction.stringToPubKey(prop) -> value.toLong }
      val signatures = rawSignatures.map { case (prop, sig) => Transaction.stringToPubKey(prop) -> Transaction.stringToSignature(sig) }
      val issuer = Transaction.stringToPubKey(rawIssuer)

      AssetTransfer(from, to, signatures, issuer, assetCode, fee, timestamp, data)
    }
}