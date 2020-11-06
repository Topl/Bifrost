package co.topl.modifier.transaction

import java.time.Instant

import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
import co.topl.attestation.proof.SignatureCurve25519
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.modifier.transaction
import co.topl.modifier.transaction.Transaction.{ Nonce, Value }
import co.topl.nodeView.state.box.{ AssetBox, TokenBox }
import com.google.common.primitives.{ Bytes, Ints }
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor }
import scorex.crypto.hash.Blake2b256

import scala.util.{ Failure, Success, Try }

case class AssetTransfer (override val from      : IndexedSeq[(PublicKeyCurve25519Proposition, Nonce)],
                          override val to        : IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
                          override val signatures: Map[PublicKeyCurve25519Proposition, SignatureCurve25519],
                          issuer                 : PublicKeyCurve25519Proposition,
                          assetCode              : String,
                          override val fee       : Long,
                          override val timestamp : Long,
                          override val data      : String
                         ) extends TransferTransaction(from, to, signatures, fee, timestamp, data) {

  override lazy val messageToSign: Array[Byte] = Bytes.concat(
    "AssetTransfer".getBytes(),
    super.messageToSign,
    issuer.pubKeyBytes,
    assetCode.getBytes,
    )

  override lazy val newBoxes: Traversable[AssetBox] = to
    .filter(toInstance => toInstance._2 > 0L)
    .zipWithIndex
    .map {
      case ((prop, value), idx) =>
        val nonce = Transaction.nonceFromDigest(Blake2b256(
          "AssetTransfer".getBytes ++
            prop.pubKeyBytes ++
            issuer.pubKeyBytes ++
            assetCode.getBytes ++
            hashNoNonces ++
            Ints.toByteArray(idx)
          ))
        AssetBox(prop, nonce, value, assetCode, issuer, data)
    }

  override def toString: String = s"AssetTransfer(${json.noSpaces})"
}

object AssetTransfer extends TransferCompanion {

  implicit val jsonEncoder: Encoder[AssetTransfer] = { tx: AssetTransfer =>
    Map(
      "txHash" -> tx.id.asJson,
      "txType" -> "AssetTransfer".asJson,
      "newBoxes" -> tx.newBoxes.map(_.json).toSeq.asJson,
      "boxesToRemove" -> tx.boxIdsToOpen.asJson,
      "from" -> tx.from.asJson,
      "to" -> tx.to.asJson,
      "signatures" -> tx.signatures.asJson,
      "fee" -> tx.fee.asJson,
      "timestamp" -> tx.timestamp.asJson,
      "data" -> tx.data.asJson,
      "issuer" -> tx.issuer.toString.asJson,
      "assetCode" -> tx.assetCode.asJson,
      ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetTransfer] = ( c: HCursor ) =>
    for {
      from <- c.downField("from").as[IndexedSeq[(PublicKeyCurve25519Proposition, Long)]]
      to <- c.downField("to").as[IndexedSeq[(PublicKeyCurve25519Proposition, Long)]]
      signatures <- c.downField("signatures").as[Map[PublicKeyCurve25519Proposition, SignatureCurve25519]]
      fee <- c.downField("fee").as[Long]
      timestamp <- c.downField("timestamp").as[Long]
      data <- c.downField("data").as[String]
      issuer <- c.downField("issuer").as[PublicKeyCurve25519Proposition]
      assetCode <- c.downField("assetCode").as[String]
    } yield {
      AssetTransfer(from, to, signatures, issuer, assetCode, fee, timestamp, data)
    }

  /**
   *
   * @param from
   * @param to
   * @param issuer
   * @param assetCode
   * @param fee
   * @param timestamp
   * @param data
   * @return
   */
  def apply (from     : IndexedSeq[(PrivateKeyCurve25519, Nonce)],
             to       : IndexedSeq[(PublicKeyCurve25519Proposition, Value)],
             issuer   : PublicKeyCurve25519Proposition,
             assetCode: String,
             fee      : Long,
             timestamp: Long,
             data     : String
            ): AssetTransfer = {

    val params = parametersForApply(from, to, fee, timestamp, "AssetTransfer", issuer, assetCode, data).get
    transaction.AssetTransfer(params._1, to, params._2, issuer, assetCode, fee, timestamp, data)
  }

  /**
   *
   * @param stateReader
   * @param toReceive
   * @param sender
   * @param issuer
   * @param assetCode
   * @param fee
   * @param data
   * @param assetId
   * @return
   */
  def createRaw (stateReader: SR,
                 toReceive  : IndexedSeq[(PublicKeyCurve25519Proposition, Long)],
                 sender     : IndexedSeq[PublicKeyCurve25519Proposition],
                 issuer     : PublicKeyCurve25519Proposition,
                 assetCode  : String,
                 fee        : Long,
                 data       : String,
                 assetId    : Option[String] = None
                 ): Try[AssetTransfer] = Try {
    val params = parametersForCreate(stateReader, toReceive, sender, fee, "AssetTransfer", issuer, assetCode, assetId)
    val timestamp = Instant.now.toEpochMilli
    AssetTransfer(params._1.map(t => t._1 -> t._2), params._2, Map(), issuer, assetCode, fee, timestamp, data)
  }

  /**
   *
   * @param tx
   * @return
   */
  def validatePrototype ( tx: AssetTransfer ): Try[Unit] = syntacticValidateTransfer(tx, withSigs = false)

  /**
   *
   * @param tx
   * @return
   */
  def syntacticValidate ( tx: AssetTransfer ): Try[Unit] = syntacticValidateTransfer(tx)

  /**
   *
   * @param tx
   * @param state
   * @return
   */
  def semanticValidate ( tx: AssetTransfer, state: SR ): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx) match {
      case Failure(e) => throw e
      case _          => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value).sum
    val unlockers = TokenBox.generateUnlockers(tx.from, tx.signatures)

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
}