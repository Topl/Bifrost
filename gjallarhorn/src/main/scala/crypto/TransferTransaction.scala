package crypto

import attestation.AddressEncoder.NetworkPrefix
import attestation.{Address, Evidence, EvidenceProducer, Proof, Proposition,
  PublicKeyPropositionCurve25519, SignatureCurve25519, ThresholdPropositionCurve25519, ThresholdSignatureCurve25519}
import crypto.ModifierId.ModifierTypeId
import crypto.TransferTransaction.BoxParams
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import com.google.common.primitives.{Ints, Longs}
import scorex.crypto.hash.Blake2b256
import modifier.BoxId
import utils.{Identifiable, Identifier}

import scala.collection.mutable.{Map => MMap}
import scala.util.Try


case class TransferTransaction[P <: Proposition: EvidenceProducer: Identifiable]
( from:        IndexedSeq[(Address, Long)],
  to:          IndexedSeq[(Address, TokenValueHolder)],
  attestation: Map[P, Proof[P]],
   fee:         Long,
   timestamp:   Long,
   data:        Option[String],
   minting:     Boolean,
   txType:      String
 ) {

  lazy val id: ModifierId = ModifierId(this)

  val newBoxes: Traversable[Box] = {
    val params = TransferTransaction.boxParams(this)

    val feeChangeBox =
      if (fee > 0L) Traversable(Box(params._1.evidence, params._1.nonce, "PolyBox", params._1.value))
      else Traversable()

    val boxes = params._2.map {
      case BoxParams(ev, n, v) => Box(ev, n, txType, v)
      case _                   => throw new Error("Attempted application of invalid value holder")
    }

    feeChangeBox ++ boxes
  }

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  def getPropIdentifier: Identifier = Identifiable[P].getId

  def messageToSign: Array[Byte] =
      data.fold(Array(0: Byte))(_.getBytes) :+ (if (minting) 1: Byte else 0: Byte)

}

object TransferTransaction {

  case class BoxParams[T <: TokenValueHolder](evidence: Evidence, nonce: Long, value: T)

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (2: Byte)

  /** Computes a unique nonce value based on the transaction type and
    * inputs and returns the details needed to create the output boxes for the transaction
    */
  def boxParams[
    T <: TokenValueHolder,
    P <: Proposition
  ](tx: TransferTransaction[P]): (BoxParams[SimpleValue], Traversable[BoxParams[TokenValueHolder]]) = {
    // known input data (similar to messageToSign but without newBoxes since they aren't known yet)

    val typePrefix = tx.txType match {
      case "ArbitTransfer" => 1: Byte
      case "PolyTransfer" => 2: Byte
      case "AssetTransfer" => 3: Byte
    }

    val inputBytes =
      Array(typePrefix) ++
        tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes) ++
        Longs.toByteArray(tx.timestamp) ++
        Longs.toByteArray(tx.fee)

    def calcNonce(index: Int): Long = {
      val digest = Blake2b256(inputBytes ++ Ints.toByteArray(index))
      Longs.fromByteArray(digest.take(Longs.BYTES))
    }

    val feeChangeParams = BoxParams(tx.to.head._1.evidence, calcNonce(0), SimpleValue(tx.to.head._2.quantity))

    val outputParams = tx.to.tail
      .filter(_._2.quantity > 0L)
      .zipWithIndex
      .map { case ((addr, value), idx) =>
        BoxParams(addr.evidence, calcNonce(idx + 1), value)
      }

    (feeChangeParams, outputParams)
  }

  /** Retrieves the boxes from state for the specified sequence of senders and
    * filters them based on the type of transaction */
  private def getSenderBoxesForTx(walletBoxes: MMap[Address, MMap[BoxId, Box]],
                                   sender:    IndexedSeq[Address],
                                   txType:    String,
                                   assetArgs: Option[(AssetCode, Boolean)] = None
                                 ): Map[String, IndexedSeq[(String, Address, Box)]] = {
    sender.flatMap(s => {
      val bxs = walletBoxes.getOrElse(s, throw new Exception("No boxes found to fund transaction"))

      bxs.map(bx => bx._2.typeOfBox match {
        // always get polys because this is how fees are paid
        case "PolyBox" => ("Poly", s, bx._2)

        case "ArbitBox" => ("Arbit", s, bx._2)

        case "AssetBox" =>
          bx._2.value match {
            case token: AssetValue if txType == "AssetTransfer" && assetArgs.forall(_._1 == token.assetCode) =>
              ("Asset", s, bx._2)
          }
      }).toIndexedSeq
    }).groupBy(_._1)
  }

  def getInputsOutputs[
    T <: TokenValueHolder
  ](txType: String,
    polyBalance: Long,
    fee: Long,
    senderBoxes: Map[String, IndexedSeq[(String, Address, Box)]],
    changeAddress: Address,
    toReceive: IndexedSeq[(Address, T)],
    consolidationAddress: Option[Address],
    assetArgs: Option[(AssetCode, Boolean)] = None // (assetCode, minting)
      ): (Long, IndexedSeq[(Address, Long)], IndexedSeq[(Address, TokenValueHolder)]) = {

    // compute the amount of tokens that will be sent to the recipients
    val amtToSpend = toReceive.map(_._2.quantity).sum

    txType match {
      case "PolyTransfer" =>
        (
          polyBalance - fee,
          senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          (changeAddress, SimpleValue(polyBalance - fee - amtToSpend)) +: toReceive
        )

      case "ArbitTransfer" =>
        val arbitBalance =
          senderBoxes
            .getOrElse("Arbit", throw new Exception(s"No Arbit funds available for the transaction"))
            .map(_._3.value.quantity)
            .sum

        (
          arbitBalance,
          senderBoxes("Arbit").map(bxs => (bxs._2, bxs._3.nonce)) ++
            senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          IndexedSeq((changeAddress, SimpleValue(polyBalance - fee)),
            (consolidationAddress.getOrElse(changeAddress), SimpleValue(arbitBalance - amtToSpend))
          ) ++
            toReceive
        )

      // case for minting asset transfers
      // todo - JAA - what happens here when I specify a zero fee and use the same timestamp?
      // need to check that unique outputs are generated but I am not sure they will be because the tx
      // bytes will be the same so the nonce will end up being the same?
      case "AssetTransfer" if assetArgs.forall(_._2) =>
        (
          Long.MaxValue,
          senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          (changeAddress, SimpleValue(polyBalance - fee)) +: toReceive
        )

      // todo: JAA - we need to handle the case where the change output is zero. I think this
      // means we should move these functions to their singleton objects and define the handling there
      case "AssetTransfer" =>
        val assetBalance =
          senderBoxes
            .getOrElse("Asset", throw new Exception(s"No Assets found with assetCode ${assetArgs.get._1}"))
            .map(_._3.value.quantity)
            .sum

        (
          assetBalance,
          senderBoxes("Asset").map(bxs => (bxs._2, bxs._3.nonce)) ++
            senderBoxes("Poly").map(bxs => (bxs._2, bxs._3.nonce)),
          IndexedSeq((changeAddress, SimpleValue(polyBalance - fee)),
            (consolidationAddress.getOrElse(changeAddress), AssetValue(assetBalance - amtToSpend, assetArgs.get._1))
          ) ++
            toReceive
        )
    }
  }

  /** Determines the input boxes needed to create a transfer transaction
    * @param toReceive the recipients of boxes
    * @param sender the set of addresses that will contribute boxes to this transaction
    * @param fee the fee to be paid for the transaction
    * @param txType the type of transfer
    * @param assetArgs a tuple of asset specific details for finding the right asset boxes to be sent in a transfer
    * @return the input box information and output data needed to create the transaction case class
    */
  def createRawTransferParams[
    T <: TokenValueHolder
  ](walletBoxes: MMap[Address, MMap[BoxId, Box]],
    toReceive:            IndexedSeq[(Address, T)],
    sender:               IndexedSeq[Address],
    changeAddress:        Address,
    consolidationAddress: Option[Address],
    fee:                  Long,
    txType:               String,
    assetArgs:            Option[(AssetCode, Boolean)] = None // (assetCode, minting)
   ): Try[(IndexedSeq[(Address, Long)], IndexedSeq[(Address, TokenValueHolder)])] = Try {

    // Lookup boxes for the given senders
    val senderBoxes = getSenderBoxesForTx(walletBoxes, sender, txType, assetArgs)

    // compute the Poly balance since it is used often
    val polyBalance = senderBoxes
      .getOrElse("Poly", throw new Exception(s"No Poly funds available for the transaction fee payment"))
        .map(_._3.value.quantity).sum
    // ensure there are enough polys to pay the fee
    require(polyBalance >= fee, s"Insufficient funds available to pay transaction fee.")

    // compute the amount of tokens that will be sent to the recipients
    val amtToSpend = toReceive.map(_._2.quantity).sum

    // create the list of inputs and outputs (senderChangeOut & recipientOut)
    val (availableToSpend, inputs, outputs) =
      getInputsOutputs(txType, polyBalance, fee, senderBoxes, changeAddress, toReceive, consolidationAddress, assetArgs)

    // ensure there are sufficient funds from the sender boxes to create all outputs
    require(availableToSpend >= amtToSpend, "Insufficient funds available to create transaction.")

    (inputs, outputs)
  }

  //For passing Longs to front-end/Javascript
  def encodeFrom(from: IndexedSeq[(Address, Long)]): Json = {
    from.map(x => (x._1.asJson, x._2.toString.asJson)).asJson
  }

  implicit def jsonEncoder[P <: Proposition]: Encoder[TransferTransaction[P]] = { tx: TransferTransaction[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> tx.txType.asJson,
      "propositionType" -> tx.getPropIdentifier.typeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> encodeFrom(tx.from),
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson,
      "timestamp"       -> tx.timestamp.asJson,
      "minting"         -> tx.minting.asJson,
      "data"            -> tx.data.asJson
    ).asJson
  }

  implicit def jsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[TransferTransaction[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Long)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, SimpleValue)]]
        fee       <- c.downField("fee").as[Long]
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[String]]
        propType  <- c.downField("propositionType").as[String]
        minting    <- c.downField("txType").as[Boolean]
        txType    <- c.downField("txType").as[String]
      } yield {

        (propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            c.downField("signatures").as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]].map {
              new TransferTransaction
                [PublicKeyPropositionCurve25519](from, to, _, fee, timestamp, data, minting, txType)
            }

          case ThresholdPropositionCurve25519.`typeString` =>
            c.downField("signatures").as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]].map {
              new TransferTransaction
                [ThresholdPropositionCurve25519](from, to, _, fee, timestamp, data, minting, txType)
            }
        }) match {
          case Right(tx) => tx
          case Left(ex)  => throw ex
        }
      }

}

