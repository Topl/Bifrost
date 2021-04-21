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
) extends Transaction[TokenValueHolder, P] {

  lazy val bloomTopics: IndexedSeq[BloomTopic] = to.map(b => BloomTopic @@ b._1.bytes)

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (addr, nonce) =>
    BoxId.idFromEviNonce(addr.evidence, nonce)
  }

  val (feeOutputParams, coinOutputParams) = TransferTransaction.calculateBoxNonce[T](this, to)

  val feeChangeOutput: PolyBox =
    PolyBox(feeOutputParams.evidence, feeOutputParams.nonce, feeOutputParams.value)

  val coinOutput: Traversable[TokenBox[T]]

  override val newBoxes: Traversable[TokenBox[TokenValueHolder]]

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

  case class BoxParams[+T <: TokenValueHolder](evidence: Evidence, nonce: Box.Nonce, value: T)

  case class TransferCreationState(
    senderBoxes: Map[String, IndexedSeq[(String, Address, TokenBox[TokenValueHolder])]],
    polyBalance: Int128
  )

  def encodeFrom(from: IndexedSeq[(Address, Box.Nonce)]): Json =
    from.map(x => (x._1.asJson, x._2.toString.asJson)).asJson

  /** Computes a unique nonce value based on the transaction type and
    * inputs and returns the details needed to create the output boxes for the transaction
    */
  def calculateBoxNonce[T <: TokenValueHolder](
    tx: TransferTransaction[T, _ <: Proposition],
    to: IndexedSeq[(Address, T)]
  ): (BoxParams[SimpleValue], Traversable[BoxParams[T]]) = {

    // known input data (similar to messageToSign but without newBoxes since they aren't known yet)
    val txIdPrefix = Transaction.identifier(tx).typePrefix
    val boxIdsToOpenAccumulator = tx.boxIdsToOpen.foldLeft(Array[Byte]())((acc, x) => acc ++ x.hashBytes)
    val timestampBytes = Longs.toByteArray(tx.timestamp)
    val feeBytes = tx.fee.toByteArray

    val inputBytes =
      Array(txIdPrefix) ++ boxIdsToOpenAccumulator ++ timestampBytes ++ feeBytes

    val calcNonce: Int => Box.Nonce = (index: Int) => {
      val digest = Blake2b256(inputBytes ++ Ints.toByteArray(index))
      Transaction.nonceFromDigest(digest)
    }

    val feeChangeParams = BoxParams(tx.to.head._1.evidence, calcNonce(0), SimpleValue(tx.to.head._2.quantity))

    val coinOutputParams: IndexedSeq[BoxParams[T]] =
      to.tail.zipWithIndex
        .map { case ((addr, value), idx) =>
          BoxParams(addr.evidence, calcNonce(idx + 1), value)
        }
    (feeChangeParams, coinOutputParams)
  }

  /** Retrieves the boxes from state for the specified sequence of senders and filters them based on the type of transaction */
  private def getSenderBoxesForTx(
    boxReader:   BoxReader[ProgramId, Address],
    sender:      IndexedSeq[Address],
    returnBoxes: String,
    assetCode:   Option[AssetCode] = None
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

            case bx: ArbitBox if returnBoxes == "Arbits" =>
              ("Arbit", s, bx)

            case bx: AssetBox if returnBoxes == "Assets" && assetCode.forall(_ == bx.value.assetCode) =>
              ("Asset", s, bx)
          }
      }
      .groupBy(_._1)

  /** Determines the input boxes needed to create a transfer transaction
    *
    * @param boxReader a read-only version of the nodes current state
    * @param sender the set of addresses that will contribute boxes to this transaction
    * @param fee the fee to be paid for the transaction
    * @param txType the type of transfer
    * @param assetCode an asset specific detail for finding the right asset boxes to be sent in a transfer
    * @return the input box information and output data needed to create the transaction case class
    */
  def getSenderBoxesAndCheckPolyBalance(
    boxReader: BoxReader[ProgramId, Address],
    sender:    IndexedSeq[Address],
    fee:       Int128,
    txType:    String,
    assetCode: Option[AssetCode] = None // (assetCode)
  ): Try[TransferCreationState] = Try {

    // Lookup boxes for the given senders
    val senderBoxes: Map[String, IndexedSeq[(String, Address, TokenBox[TokenValueHolder])]] =
      getSenderBoxesForTx(boxReader, sender, txType, assetCode)

    // compute the Poly balance since it is used often
    val polyBalance =
      senderBoxes
        .getOrElse("Poly", throw new Exception(s"No Poly funds available for the transaction fee payment"))
        .map(_._3.value.quantity)
        .sum

    // ensure there are enough polys to pay the fee
    require(polyBalance >= fee, s"Insufficient funds available to pay transaction fee.")

    TransferCreationState(senderBoxes, polyBalance)
  }

  /** Syntactic validation of a transfer transaction
    *
    * @param transaction an instance of a transaction to check
    * @param hasAttMap boolean flag controlling whether signature verification should be checked or skipped
    * @return success or failure indicating the validity of the transaction
    */
  // todo: JAA - this is the sort of transaction that should be validated, because I want to fix all of my error at once
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
    * @param transaction the transaction to check
    * @param boxReader the state to check the validity against
    * @return a success or failure denoting the result of this check
    */
  // todo: JAA - this is the sort of validation that should fail eagerly (since it can be expensive)
  def semanticValidate[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](transaction:   TransferTransaction[T, P], boxReader: BoxReader[ProgramId, Address])(implicit
    networkPrefix: NetworkPrefix
  ): Try[Unit] = {

    // compute transaction values used for validation
    val txOutput = transaction.newBoxes.map(b => b.value.quantity).sum
    val unlockers = BoxUnlocker.generate(transaction.from, transaction.attestation)

    val inputBoxes = unlockers.map { u =>
      u -> boxReader.getBox(u.closedBoxId)
    }

    val sumOfPolyInputs = inputBoxes.collect { case (_, Some(PolyBox(_, _, value))) =>
      value.quantity
    }.sum

    // check that the transaction is correctly formed before checking state
    lazy val syntacticResult =
      syntacticValidate(transaction).toEither.leftMap(e => new Exception(e.head.toString)).toTry

    // enforce transaction specific requirements
    // must provide input state to consume in order to generate new state
    lazy val txSpecific = Try {
      transaction match {
        case _: PolyTransfer[_] if transaction.minting  => // Poly block rewards (skip enfocring)
        case _: ArbitTransfer[_] if transaction.minting => // Arbit block rewards (skip enforcing)

        case _: PolyTransfer[_] =>
          require(
            sumOfPolyInputs - transaction.fee == txOutput,
            s"PolyTransfer output value does not equal input value for non-minting transaction. " +
            s"$txOutput != ${sumOfPolyInputs - transaction.fee}"
          )

        case _ =>
          /*  This case enforces that the poly input balance must equal the poly output balance

          This case is special for AssetTransfer and ArbitTransfer (collapsed to one to not duplicate the code)
          It assumes that syntactic validate enforces
            - at least one box in the `from` field
            - at least two boxes in the `to` field [changeOutput,
          then we should have a non-zero value of `sumOfPolyInputs` (if we don't they didn't provide a poly input that is required)
          and we should have the first element of the `to` list that is designated to be the feeChangeOutput (even if that output is zero)
          these two invariants (for these two transaction) allow us to make the requirement below that enforces that
          the sum of the poly inputs equals the sum of the poly outputs.
           */

          require(
            sumOfPolyInputs - transaction.fee == transaction.feeChangeOutput.value.quantity,
            s"feeChangeOutput value does not equal input value for non-minting transaction. " +
            s"${transaction.feeChangeOutput.value.quantity} != ${sumOfPolyInputs - transaction.fee}"
          )
      }
    }

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    lazy val accessibleFunds = inputBoxes
      .foldLeft[Try[Int128]](Success[Int128](0)) { case (trySum, (unlocker, boxOpt)) =>
        trySum.flatMap { partialSum =>
          boxOpt match {
            case Some(box: TokenBox[_]) if unlocker.boxKey.isValid(unlocker.proposition, transaction.messageToSign) =>
              Success(partialSum + box.value.quantity)

            case Some(_) => Failure(new Exception("Invalid unlocker"))
            case None    => Failure(new Exception(s"Box for unlocker $unlocker cannot be found in state"))
            case _       => Failure(new Exception("Invalid Box type for this transaction"))
          }
        }
      } match {
      // a normal transfer will fall in this case
      case Success(sum: Int128) if txOutput == sum - transaction.fee =>
        Success(())

      // a minting transaction (of either Arbit, Polys, or Assets) will fall in this case
      case Success(_: Int128) if transaction.minting =>
        Success(())

      case Success(sum: Int128) if !transaction.minting && txOutput != sum - transaction.fee =>
        Failure(
          new Exception(
            s"Tx output value does not equal input value for non-minting transaction. $txOutput != ${sum - transaction.fee}"
          )
        )

      case Failure(e) => Failure(e)
    }

    for {
      _ <- syntacticResult
      _ <- txSpecific
      _ <- accessibleFunds
    } yield Success(())
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
