package co.topl.modifier.transaction

import java.time.Instant

import co.topl.attestation._
import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TxType
import co.topl.modifier.transaction.TransferTransaction.{BoxParams, encodeFrom}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.codecs.Int128Codec
import co.topl.utils.{Identifiable, Identifier, Int128}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

import scala.util.Try

case class AssetTransfer[
  P <: Proposition: EvidenceProducer: Identifiable
](
  override val from:        IndexedSeq[(Address, Box.Nonce)],
  override val to:          IndexedSeq[(Address, TokenValueHolder)],
  override val attestation: Map[P, Proof[P]],
  override val fee:         Int128,
  override val timestamp:   Long,
  override val data:        Option[String] = None,
  override val minting:     Boolean = false
) extends TransferTransaction[TokenValueHolder, P](from, to, attestation, fee, timestamp, data, minting) {

  override lazy val newBoxes: Traversable[TokenBox[TokenValueHolder]] = {
    if (to.map(_._2.quantity).sum == 0 && fee == 0) Traversable()
    else {
      val params = TransferTransaction.calculateBoxNonce(this)

      val feeChangeBox =
        if (fee > 0L) Traversable(PolyBox(params._1.evidence, params._1.nonce, params._1.value))
        else Traversable()

      val assetBoxes = params._2.map {
        case BoxParams(ev, n, v: AssetValue) => AssetBox(ev, n, v)
        case _ => throw new Error("Attempted application of invalid value holder")
      }

      feeChangeBox ++ assetBoxes
    }
  }
}

object AssetTransfer {
  val typePrefix: TxType = 3: Byte
  val typeString: String = "AssetTransfer"

  implicit def identifier[P <: Proposition]: Identifiable[AssetTransfer[P]] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  /** @param boxReader
    * @param toReceive
    * @param sender
    * @param fee
    * @param data
    * @return
    */
  def createRaw[
    P <: Proposition: EvidenceProducer: Identifiable
  ](
    boxReader:            BoxReader[ProgramId, Address],
    toReceive:            IndexedSeq[(Address, AssetValue)],
    sender:               IndexedSeq[Address],
    changeAddress:        Address,
    consolidationAddress: Option[Address],
    fee:                  Int128,
    data:                 Option[String],
    minting:              Boolean
  ): Try[AssetTransfer[P]] = {

    val assetCode =
      toReceive
        .map(_._2.assetCode)
        .toSet
        .ensuring(_.size == 1, s"Found multiple asset codes when only one was expected")
        .head

    TransferTransaction
      .createRawTransferParams(boxReader, sender, fee, "Assets", Some(assetCode))
      .map { txState =>

        // compute the amount of tokens that will be sent to the recipients
        val amtToSpend = toReceive.map(_._2.quantity).sum


        // create the list of inputs and outputs (senderChangeOut & recipientOut)
        val (availableToSpend, inputs, outputs) =

          // case for minting asset transfers
          // todo - JAA - what happens here when I specify a zero fee and use the same timestamp?
          // need to check that unique outputs are generated but I am not sure they will be because the tx
          // bytes will be the same so the nonce will end up being the same?
          if (minting) {
            (
              Int128.MaxValue,
              txState.senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
              (changeAddress, SimpleValue(txState.polyBalance - fee)) +: toReceive
            )
          }

          // todo: JAA - we need to handle the case where the change output is zero.
          else {
            val assetBalance =
              txState.senderBoxes
                .getOrElse("Asset", throw new Exception(s"No Assets found with assetCode $assetCode"))
                .map(_._3.value.quantity)
                .sum

            (
              assetBalance,
              txState.senderBoxes("Asset").map(bxs => (bxs._2, bxs._3.nonce)) ++
                txState.senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
              IndexedSeq(
                (changeAddress, SimpleValue(txState.polyBalance - fee)),
                (consolidationAddress.getOrElse(changeAddress), AssetValue(assetBalance - amtToSpend, assetCode))
              ) ++ toReceive
            )
          }

        // ensure there are sufficient funds from the sender boxes to create all outputs
        require(availableToSpend >= amtToSpend, "Insufficient funds available to create transaction.")

        AssetTransfer[P](inputs, outputs, Map(), fee, Instant.now.toEpochMilli, data, minting)
      }
  }

  implicit def jsonEncoder[P <: Proposition]: Encoder[AssetTransfer[P]] = { tx: AssetTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "AssetTransfer".asJson,
      "propositionType" -> tx.getPropIdentifier.typeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> encodeFrom(tx.from),
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson(Int128Codec.jsonEncoder),
      "timestamp"       -> tx.timestamp.asJson,
      "data"            -> tx.data.asJson,
      "minting"         -> tx.minting.asJson
    ).asJson
  }

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[AssetTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, TokenValueHolder)]]
        fee       <- c.get[Int128]("fee")(Int128Codec.jsonDecoder)
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[String]]
        minting   <- c.downField("minting").as[Boolean]
        propType  <- c.downField("propositionType").as[String]
      } yield (propType match {
        case PublicKeyPropositionCurve25519.`typeString` =>
          c.downField("signatures").as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]].map {
            new AssetTransfer[PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }

        case ThresholdPropositionCurve25519.`typeString` =>
          c.downField("signatures").as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
            new AssetTransfer[ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data, minting)
          }
      }) match {
        case Right(tx) => tx
        case Left(ex)  => throw ex
      }
}
