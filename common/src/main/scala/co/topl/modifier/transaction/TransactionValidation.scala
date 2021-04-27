package co.topl.modifier.transaction

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._
import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.attestation.{Address, EvidenceProducer, Proposition}
import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.utils.Extensions.StringOps
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait AsSyntacticallyValidatableOps {

  implicit def asTransferTransactionSyntacticallyValidatableOps[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](
    tx: TransferTransaction[T, P]
  ): TransferTransactionSyntacticallyValidatableOps[T, P] =
    new TransferTransactionSyntacticallyValidatableOps(tx)
}

object AsSyntacticallyValidatableOps extends AsSyntacticallyValidatableOps

class TransferTransactionSyntacticallyValidatableOps[T <: TokenValueHolder, P <: Proposition: EvidenceProducer](
  tx: TransferTransaction[T, P]
) {

  import AsSyntacticallyValidatableOps._

  def syntacticValidation(implicit
    networkPrefix: NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    rawSyntacticValidation.andThen(_.attestationValidation)

  def rawSyntacticValidation: Validated[NonEmptyChain[SyntacticValidationFailure], TransferTransaction[T, P]] =
    validationByTransactionType
      .andThen(_.feeValidation)
      .andThen(_.timestampValidation)
      .andThen(_.dataValidation)
      .andThen(_.inputOutputBoxesUniqueValidation)

  def validationByTransactionType: ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    tx match {
      case _: ArbitTransfer[_] | _: PolyTransfer[_] if tx.minting =>
        tx.validNec
      case _: PolyTransfer[_] =>
        Validated.condNec(tx.from.nonEmpty, tx, NoInputBoxesSpecified)
      case _: ArbitTransfer[_] | _: AssetTransfer[_] =>
        Validated
          .condNec(tx.from.nonEmpty, tx, NoInputBoxesSpecified)
          .andThen(tx => Validated.condNec(tx.to.size >= 2, tx, NonPolyTxInsufficientOutputs))
    }

  def dataValidation: ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    tx.data.fold(tx.validNec[SyntacticValidationFailure])(data =>
      Validated
        .fromOption(data.getValidLatin1Bytes, DataNotLatin1)
        .toValidatedNec[SyntacticValidationFailure, Array[Byte]]
        .andThen(bytes => Validated.condNec(bytes.length <= 128, tx, DataTooLong))
    )

  def attestationValidation(implicit
    networkPrefix: NetworkPrefix
  ): Validated[NonEmptyChain[SyntacticValidationFailure], TransferTransaction[T, P]] = {
    val attestationKeys = tx.attestation.keys
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
            attestationKeys.map(_.generateEvidence).toSeq.contains(addr.evidence)
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
                  attestationKeys.map(_.address).toSeq.contains(asset.assetCode.issuer),
                  tx,
                  MintingMissingIssuersSignature
                )
              case _ => tx.validNec[SyntacticValidationFailure]
            }
            .foldLeft(tx.validNec[SyntacticValidationFailure]) { case (acc, v) => acc.andThen(_ => v) }
        case tx =>
          tx.validNec[SyntacticValidationFailure]
      }
  }

  def inputOutputBoxesUniqueValidation: ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    Validated.condNec(
      tx.newBoxes.size == tx.newBoxes.toSet.size &&
      tx.boxIdsToOpen.size == tx.boxIdsToOpen.toSet.size &&
      tx.newBoxes.map(_.id).toSet.intersect(tx.boxIdsToOpen.toSet).isEmpty,
      tx,
      InputOutputBoxesNotUnique
    )

  def feeValidation: ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    Validated.condNec(tx.fee >= 0L, tx, NegativeFeeFailure)

  def timestampValidation: ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    Validated.condNec(tx.timestamp >= 0L, tx, InvalidTimestamp)
}

sealed abstract class SyntacticValidationFailure
case object NegativeFeeFailure extends SyntacticValidationFailure
case object NoInputBoxesSpecified extends SyntacticValidationFailure
case object InvalidSendAmount extends SyntacticValidationFailure
case object InvalidTimestamp extends SyntacticValidationFailure
case object DataNotLatin1 extends SyntacticValidationFailure
case object DataTooLong extends SyntacticValidationFailure
case object UnsatisfiedProposition extends SyntacticValidationFailure
case object PropositionEvidenceMismatch extends SyntacticValidationFailure
case object MintingMissingIssuersSignature extends SyntacticValidationFailure
case object InputOutputBoxesNotUnique extends SyntacticValidationFailure
case object NonPolyTxInsufficientOutputs extends SyntacticValidationFailure

trait AsSemanticallyValidatableOps {

  implicit def asTransferTransactionSemanticallyValidatableOps[
    T <: TokenValueHolder,
    P <: Proposition: EvidenceProducer
  ](
    tx: TransferTransaction[T, P]
  ): TransferTransactionSemanticallyValidatableOps[T, P] =
    new TransferTransactionSemanticallyValidatableOps(tx)
}

object AsSemanticallyValidatableOps extends AsSemanticallyValidatableOps

class TransferTransactionSemanticallyValidatableOps[T <: TokenValueHolder, P <: Proposition: EvidenceProducer](
  tx: TransferTransaction[T, P]
) {
  import AsSyntacticallyValidatableOps._

  /** Checks the stateful validity of a transaction
    *
    * @param boxReader the state to check the validity against
    * @return a success or failure denoting the result of this check
    */
  // todo: JAA - this is the sort of validation that should fail eagerly (since it can be expensive)
  def semanticValidation(boxReader: BoxReader[ProgramId, Address])(implicit networkPrefix: NetworkPrefix): Try[Unit] = {

    // compute transaction values used for validation
    val txOutput = tx.newBoxes.map(b => b.value.quantity).sum
    val unlockers = BoxUnlocker.generate(tx.from, tx.attestation)

    val inputBoxes = unlockers.map { u =>
      u -> boxReader.getBox(u.closedBoxId)
    }

    val sumOfPolyInputs = inputBoxes.collect { case (_, Some(PolyBox(_, _, value))) =>
      value.quantity
    }.sum

    // check that the transaction is correctly formed before checking state
    lazy val syntacticResult =
      tx.syntacticValidation.toEither
        .leftMap(e => new Exception(e.head.toString))
        .toTry

    // enforce transaction specific requirements
    // must provide input state to consume in order to generate new state
    lazy val txSpecific = Try {
      tx match {
        case _: PolyTransfer[_] if tx.minting  => // Poly block rewards (skip enfocring)
        case _: ArbitTransfer[_] if tx.minting => // Arbit block rewards (skip enforcing)

        case _: PolyTransfer[_] =>
          require(
            sumOfPolyInputs - tx.fee == txOutput,
            s"PolyTransfer output value does not equal input value for non-minting transaction. " +
            s"$txOutput != ${sumOfPolyInputs - tx.fee}"
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
            sumOfPolyInputs - tx.fee == tx.feeChangeOutput.value.quantity,
            s"feeChangeOutput value does not equal input value for non-minting transaction. " +
            s"${tx.feeChangeOutput.value.quantity} != ${sumOfPolyInputs - tx.fee}"
          )
      }
    }

    // iterate through the unlockers and sum up the value of the box for each valid unlocker
    lazy val accessibleFunds = inputBoxes
      .foldLeft[Try[Int128]](Success[Int128](0)) { case (trySum, (unlocker, boxOpt)) =>
        trySum.flatMap { partialSum =>
          boxOpt match {
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

    for {
      _ <- syntacticResult
      _ <- txSpecific
      _ <- accessibleFunds
    } yield Success(())
  }
}
