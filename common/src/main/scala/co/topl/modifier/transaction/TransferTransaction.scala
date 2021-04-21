package co.topl.modifier.transaction

import cats.data.{Validated, _}
import cats.implicits._
import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.attestation.{Evidence, _}
import co.topl.modifier.BoxReader
import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.box.{Box, _}
import co.topl.utils.Extensions.StringOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Identifiable, Int128}
import com.google.common.primitives.{Ints, Longs}
import io.circe.Json
import io.circe.syntax.EncoderOps
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success, Try}

abstract class TransferTransaction[
  +T <: TokenValueHolder,
  P <: Proposition: EvidenceProducer: Identifiable
](
  val from:        IndexedSeq[(Address, Box.Nonce)],
  val to:          IndexedSeq[(Address, T)],
  val attestation: Map[P, Proof[P]],
  val fee:         Int128,
  val timestamp:   Long,
  val data:        Option[String],
  val minting:     Boolean
) extends Transaction[T, P] {

  lazy val bloomTopics: IndexedSeq[BloomTopic] = to.map(b => BloomTopic @@ b._1.bytes)

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  override def messageToSign: Array[Byte] =
    super.messageToSign ++
    data.fold(Array(0: Byte))(_.getBytes) :+ (if (minting) 1: Byte else 0: Byte)

  def semanticValidate(boxReader: BoxReader[ProgramId, Address])(implicit networkPrefix: NetworkPrefix): Try[Unit] =
    TransferTransaction.semanticValidate(this, boxReader)

  def syntacticValidate(implicit
    networkPrefix: NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    TransferTransaction.syntacticValidate(this)

  def rawValidate(implicit
    networkPrefix: NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    TransferTransaction.syntacticValidate(this, hasAttMap = false)

}

object TransferTransaction {

  case class BoxParams[T <: TokenValueHolder](evidence: Evidence, nonce: Box.Nonce, value: T)

  /** Computes a unique nonce value based on the transaction type and
    * inputs and returns the details needed to create the output boxes for the transaction
    */
  def boxParams[
    T <: TokenValueHolder,
    P <: Proposition
  ](tx: TransferTransaction[T, P]): (BoxParams[SimpleValue], Traversable[BoxParams[T]]) = {
    // known input data (similar to messageToSign but without newBoxes since they aren't known yet)
    val inputBytes =
      Array(Transaction.identifier(tx).typePrefix) ++
      tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes) ++
      Longs.toByteArray(tx.timestamp) ++
      tx.fee.toByteArray

    def calcNonce(index: Int): Box.Nonce = {
      val digest = Blake2b256(inputBytes ++ Ints.toByteArray(index))
      Transaction.nonceFromDigest(digest)
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

  def encodeFrom(from: IndexedSeq[(Address, Box.Nonce)]): Json =
    from.map(x => (x._1.asJson, x._2.toString.asJson)).asJson

  /** Retrieves the boxes from state for the specified sequence of senders and filters them based on the type of transaction */
  private def getSenderBoxesForTx(
    boxReader: BoxReader[ProgramId, Address],
    sender:    IndexedSeq[Address],
    txType:    String,
    assetArgs: Option[(AssetCode, Boolean)] = None
  ): Map[String, IndexedSeq[(String, Address, TokenBox[TokenValueHolder])]] =
    sender
      .flatMap { s =>
        boxReader
          .getTokenBoxes(s)
          .getOrElse(
            throw new Exception("No boxes found to fund transaction")
          ) // isn't this just an empty sequence instead of None?
          .collect {
            // always get polys because this is how fees are paid
            case bx: PolyBox =>
              ("Poly", s, bx)

            case bx: ArbitBox if txType == "ArbitTransfer" =>
              ("Arbit", s, bx)

            case bx: AssetBox if txType == "AssetTransfer" && assetArgs.forall(_._1 == bx.value.assetCode) =>
              ("Asset", s, bx)
          }
      }
      .groupBy(_._1)

  /** Determines the input boxes needed to create a transfer transaction
    *
    * @param boxReader a read-only version of the nodes current state
    * @param toReceive the recipients of boxes
    * @param sender the set of addresses that will contribute boxes to this transaction
    * @param fee the fee to be paid for the transaction
    * @param txType the type of transfer
    * @param assetArgs a tuple of asset specific details for finding the right asset boxes to be sent in a transfer
    * @return the input box information and output data needed to create the transaction case class
    */
  def createRawTransferParams[
    T <: TokenValueHolder
  ](
    boxReader:            BoxReader[ProgramId, Address],
    toReceive:            IndexedSeq[(Address, T)],
    sender:               IndexedSeq[Address],
    changeAddress:        Address,
    consolidationAddress: Option[Address],
    fee:                  Int128,
    txType:               String,
    assetArgs:            Option[(AssetCode, Boolean)] = None // (assetCode, minting)
  ): Try[(IndexedSeq[(Address, Box.Nonce)], IndexedSeq[(Address, TokenValueHolder)])] = Try {

    // Lookup boxes for the given senders
    val senderBoxes = getSenderBoxesForTx(boxReader, sender, txType, assetArgs)

    // compute the Poly balance since it is used often
    val polyBalance =
      senderBoxes
        .getOrElse("Poly", throw new Exception(s"No Poly funds available for the transaction fee payment"))
        .map(_._3.value.quantity)
        .sum

    // compute the amount of tokens that will be sent to the recipients
    val amtToSpend = toReceive.map(_._2.quantity).sum

    // ensure there are enough polys to pay the fee
    require(polyBalance >= fee, s"Insufficient funds available to pay transaction fee.")

    // create the list of inputs and outputs (senderChangeOut & recipientOut)
    val (availableToSpend, inputs, outputs) = txType match {
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
          IndexedSeq(
            (changeAddress, SimpleValue(polyBalance - fee)),
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
          Int128.MaxValue,
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
          IndexedSeq(
            (changeAddress, SimpleValue(polyBalance - fee)),
            (consolidationAddress.getOrElse(changeAddress), AssetValue(assetBalance - amtToSpend, assetArgs.get._1))
          ) ++
          toReceive
        )
    }

    // ensure there are sufficient funds from the sender boxes to create all outputs
    require(availableToSpend >= amtToSpend, "Insufficient funds available to create transaction.")

    (inputs, outputs)
  }

  /** Syntactic validation of a transfer transaction
    *
    * @param tx an instance of a transaction to check
    * @param hasAttMap boolean flag controlling whether signature verification should be checked or skipped
    * @return success or failure indicating the validity of the transaction
    */
  def syntacticValidate[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](tx:            TransferTransaction[T, P], hasAttMap: Boolean = true)(implicit
    networkPrefix: NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] = {
    val validationByTransactionType: ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
      tx match {
        case _: ArbitTransfer[_] | _: PolyTransfer[_] =>
          tx.validNec
        case _ =>
          NonEmptyChain(
            Validated.condNec(tx.minting == tx.fee > 0L, tx, MintingZeroFeeFailure: SyntacticValidationFailure),
            Validated.condNec(tx.fee > 0L, tx, ZeroFeeFailure: SyntacticValidationFailure),
            Validated.condNec(tx.to.forall(_._2.quantity > 0L), tx, InvalidSendAmount: SyntacticValidationFailure)
          ).reduceLeft { case (acc, f) => acc.andThen(_ => f) }
      }

    val dataValidation =
      tx.data.fold(tx.validNec[SyntacticValidationFailure])(data =>
        Validated
          .fromOption(data.getValidLatin1Bytes, DataNotLatin1: SyntacticValidationFailure)
          .toValidatedNec[SyntacticValidationFailure, Array[Byte]]
          .andThen(bytes => Validated.condNec(bytes.length <= 128, tx, DataTooLong: SyntacticValidationFailure))
      )

    val attMapValidation =
      if (hasAttMap)
        Validated
          .condNec(
            tx.attestation.forall { case (prop, proof) =>
              proof.isValid(prop, tx.messageToSign)
            },
            tx,
            UnsatisfiedProposition
          )
          .andThen(tx =>
            Validated.condNec(
              tx.from.forall { case (addr, _) =>
                tx.attestation.keys.map(_.generateEvidence).toSeq.contains(addr.evidence)
              },
              tx,
              PropositionEvidenceMismatch
            )
          )
          .andThen {
            case _: AssetTransfer[_] if tx.minting =>
              tx.to
                .map {
                  case (_, asset: AssetValue) =>
                    Validated.condNec(
                      tx.attestation.keys.map(_.address).toSeq.contains(asset.assetCode.issuer),
                      tx,
                      MintingMissingIssuersSignature: SyntacticValidationFailure
                    )
                  case (_, _: SimpleValue) => tx.validNec[SyntacticValidationFailure]
                  case _                   => InvalidValue.invalidNec[TransferTransaction[T, P]]
                }
                .foldLeft(tx.validNec[SyntacticValidationFailure]) { case (acc, v) => acc.andThen(_ => v) }
            case tx =>
              tx.validNec[SyntacticValidationFailure]
          }
      else tx.validNec[SyntacticValidationFailure]

    val inputOutputBoxesUniqueValidation =
      Validated.condNec(
        tx.newBoxes.map(_.id).toSet.intersect(tx.boxIdsToOpen.toSet).isEmpty,
        tx,
        InputOutputBoxesNotUnique: SyntacticValidationFailure
      )

    validationByTransactionType
      .andThen(tx => Validated.condNec(tx.timestamp >= 0L, tx, InvalidTimestamp))
      .andThen(_ => dataValidation)
      .andThen(_ => attMapValidation)
      .andThen(_ => inputOutputBoxesUniqueValidation)
  }

  /** Checks the stateful validity of a transaction
    *
    * @param tx the transaction to check
    * @param boxReader the state to check the validity against
    * @return a success or failure denoting the result of this check
    */
  def semanticValidate[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](tx:            TransferTransaction[T, P], boxReader: BoxReader[ProgramId, Address])(implicit
    networkPrefix: NetworkPrefix
  ): Try[Unit] = {

    // check that the transaction is correctly formed before checking state
    syntacticValidate(tx).toEither match {
      case Left(e) => throw new Exception(e.head.toString)
      case _       => // continue processing
    }

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value.quantity).sum
    val unlockers = BoxUnlocker.generate(tx.from, tx.attestation)

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    unlockers
      .foldLeft[Try[Int128]](Success[Int128](0)) { (trySum, unlocker) =>
        trySum.flatMap { partialSum =>
          boxReader.getBox(unlocker.closedBoxId) match {
            case Some(box: TokenBox[_]) if unlocker.boxKey.isValid(unlocker.proposition, tx.messageToSign) =>
              Success(partialSum + box.value.quantity)

            case Some(_) => Failure(new Exception("Invalid unlocker"))
            case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
            case _       => Failure(new Exception("Invalid Box type for this transaction"))
          }
        }
      } match {
      // a normal transfer will fall in this case
      case Success(sum: Int128) if txOutput == sum - tx.fee =>
        Success(())

      // a minting transaction (of either Arbit, Polys, or Assets) will fall in this case
      case Success(_: Int128) if tx.minting =>
        Success(())

      case Success(sum: Int128) if !tx.minting && txOutput != sum - tx.fee =>
        Failure(
          new Exception(
            s"Tx output value does not equal input value for non-minting transaction. $txOutput != ${sum - tx.fee}"
          )
        )

      case Failure(e) => Failure(e)
    }
  }
}

sealed abstract class SyntacticValidationFailure
case object ZeroFeeFailure extends SyntacticValidationFailure
case object MintingZeroFeeFailure extends SyntacticValidationFailure
case object InvalidSendAmount extends SyntacticValidationFailure
case object InvalidTimestamp extends SyntacticValidationFailure
case object DataNotLatin1 extends SyntacticValidationFailure
case object DataTooLong extends SyntacticValidationFailure
case object UnsatisfiedProposition extends SyntacticValidationFailure
case object PropositionEvidenceMismatch extends SyntacticValidationFailure
case object MintingMissingIssuersSignature extends SyntacticValidationFailure
case object InvalidValue extends SyntacticValidationFailure
case object InputOutputBoxesNotUnique extends SyntacticValidationFailure

sealed abstract class Syntactic
case object InvalidUnlocker extends Syntactic
case object BoxNotFound extends Syntactic
case object InvalidBoxTye extends Syntactic
