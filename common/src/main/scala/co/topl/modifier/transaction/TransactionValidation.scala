package co.topl.modifier.transaction

import cats.Semigroup
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._
import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.attestation.{Address, Proof, Proposition}
import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.utils.Extensions.StringOps
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix

import scala.language.implicitConversions

trait SyntacticallyValidatable[T] {

  def syntacticValidation(t: T)(implicit
    networkPrefix:           NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, T]

  def rawSyntacticValidation(t: T): ValidatedNec[SyntacticValidationFailure, T]
}

object SyntacticallyValidatable {

  trait ContravariantSyntacticallyValidatable[-T] extends Serializable {
    def show(t: T): String
  }

  trait Ops[A] {
    def typeClassInstance: SyntacticallyValidatable[A]
    def self: A

    def syntacticValidation(implicit networkPrefix: NetworkPrefix): ValidatedNec[SyntacticValidationFailure, A] =
      typeClassInstance.syntacticValidation(self)

    def rawSyntacticValidation: ValidatedNec[SyntacticValidationFailure, A] =
      typeClassInstance.rawSyntacticValidation(self)
  }

  trait ToSyntacticallyValidatableOps {

    implicit def toSyntacticValidation[T: SyntacticallyValidatable](target: T): Ops[T] =
      new Ops[T] {
        val self = target
        val typeClassInstance = implicitly[SyntacticallyValidatable[T]]
      }
  }

  trait Instances {

    implicit def polyTransferTransactionSyntacticallyValidatable[P <: Proposition]
      : SyntacticallyValidatable[PolyTransfer[P]] = {
      val delegate =
        new TransferTransactionSyntacticallyValidatable[SimpleValue, P]

      new SyntacticallyValidatable[PolyTransfer[P]] {
        override def syntacticValidation(t: PolyTransfer[P])(implicit
          networkPrefix:                    NetworkPrefix
        ): ValidatedNec[SyntacticValidationFailure, PolyTransfer[P]] =
          delegate.syntacticValidation(t).map(_ => t)

        override def rawSyntacticValidation(
          t: PolyTransfer[P]
        ): ValidatedNec[SyntacticValidationFailure, PolyTransfer[P]] =
          delegate.rawSyntacticValidation(t).map(_ => t)
      }
    }

    implicit def arbitTransferTransactionSyntacticallyValidatable[P <: Proposition]
      : SyntacticallyValidatable[ArbitTransfer[P]] = {
      val delegate =
        new TransferTransactionSyntacticallyValidatable[SimpleValue, P]

      new SyntacticallyValidatable[ArbitTransfer[P]] {
        override def syntacticValidation(t: ArbitTransfer[P])(implicit
          networkPrefix:                    NetworkPrefix
        ): ValidatedNec[SyntacticValidationFailure, ArbitTransfer[P]] =
          delegate.syntacticValidation(t).map(_ => t)

        override def rawSyntacticValidation(
          t: ArbitTransfer[P]
        ): ValidatedNec[SyntacticValidationFailure, ArbitTransfer[P]] =
          delegate.rawSyntacticValidation(t).map(_ => t)
      }
    }

    implicit def assetTransferTransactionSyntacticallyValidatable[P <: Proposition]
      : SyntacticallyValidatable[AssetTransfer[P]] = {
      val delegate =
        new TransferTransactionSyntacticallyValidatable[TokenValueHolder, P]

      new SyntacticallyValidatable[AssetTransfer[P]] {
        override def syntacticValidation(t: AssetTransfer[P])(implicit
          networkPrefix:                    NetworkPrefix
        ): ValidatedNec[SyntacticValidationFailure, AssetTransfer[P]] =
          delegate.syntacticValidation(t).map(_ => t)

        override def rawSyntacticValidation(
          t: AssetTransfer[P]
        ): ValidatedNec[SyntacticValidationFailure, AssetTransfer[P]] =
          delegate.rawSyntacticValidation(t).map(_ => t)
      }
    }

    implicit def transferTransactionSyntacticallyValidatable[T <: TokenValueHolder, P <: Proposition]
      : TransferTransactionSyntacticallyValidatable[T, P] =
      new TransferTransactionSyntacticallyValidatable[T, P]

    implicit def transactionSyntacticallyValidatable[T, P <: Proposition]: SyntacticallyValidatable[Transaction[T, P]] =
      new SyntacticallyValidatable[Transaction[T, P]] {

        override def syntacticValidation(
          t:                      Transaction[T, P]
        )(implicit networkPrefix: NetworkPrefix): ValidatedNec[SyntacticValidationFailure, Transaction[T, P]] =
          t match {
            case transaction: TransferTransaction[TokenValueHolder, P] =>
              transferTransactionSyntacticallyValidatable[TokenValueHolder, P]
                .syntacticValidation(transaction)
                .map(_ => t)
            case t => t.validNec
          }

        override def rawSyntacticValidation(
          t: Transaction[T, P]
        ): ValidatedNec[SyntacticValidationFailure, Transaction[T, P]] =
          t match {
            case transaction: TransferTransaction[TokenValueHolder, P] =>
              transferTransactionSyntacticallyValidatable[TokenValueHolder, P]
                .rawSyntacticValidation(transaction)
                .map(_ => t)
            case t => t.validNec
          }
      }
  }

}

class TransferTransactionSyntacticallyValidatable[T <: TokenValueHolder, P <: Proposition]
    extends SyntacticallyValidatable[TransferTransaction[T, P]] {

  implicit private val txSemigroup: Semigroup[TransferTransaction[T, P]] = (_, b) => b

  def syntacticValidation(tx: TransferTransaction[T, P])(implicit
    networkPrefix:            NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    rawSyntacticValidation(tx).combine(attestationValidation(tx))

  def rawSyntacticValidation(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    validationByTransactionType(tx)
      .combine(feeValidation(tx))
      .combine(timestampValidation(tx))
      .combine(dataValidation(tx))
      .combine(inputOutputBoxesUniqueValidation(tx))

  private[transaction] def validationByTransactionType(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    tx match {
      case _: ArbitTransfer[_] | _: PolyTransfer[_] if tx.minting =>
        tx.validNec
      case _: PolyTransfer[_] =>
        Validated.condNec(tx.from.nonEmpty, tx, NoInputBoxesSpecified)
      case _: ArbitTransfer[_] | _: AssetTransfer[_] =>
        Validated
          .condNec(tx.from.nonEmpty, tx, NoInputBoxesSpecified)
          .combine(Validated.condNec(tx.to.size >= 2, tx, NonPolyTxInsufficientOutputs))
    }

  private[transaction] def dataValidation(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    tx.data.fold(tx.validNec[SyntacticValidationFailure])(data =>
      Validated
        .fromOption(data.getValidLatin1Bytes, DataNotLatin1)
        .toValidatedNec[SyntacticValidationFailure, Array[Byte]]
        .andThen(bytes => Validated.condNec(bytes.length <= 128, tx, DataTooLong))
    )

  private[transaction] def propositionSatisfiedValidation(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    Validated
      .condNec(
        tx.attestation.forall { case (prop, proof) =>
          proof.isValid(prop, tx.messageToSign)
        },
        tx,
        UnsatisfiedProposition
      )

  private[transaction] def propositionEvidenceMatchValidation(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] = {
    import tx.evidenceProducerEv
    Validated.condNec(
      tx.from.forall { case (addr, _) =>
        tx.attestation.keys.map(_.generateEvidence).toSeq.contains(addr.evidence)
      },
      tx,
      PropositionEvidenceMismatch
    )
  }

  private[transaction] def mintingIssuersValidation(tx: TransferTransaction[T, P])(implicit
    networkPrefix:                                      NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    tx match {
      case _: AssetTransfer[_] if tx.minting =>
        tx.to
          .map {
            case (_, asset: AssetValue) =>
              Validated.condNec(
                tx.attestation.keys.map(_.address).toSeq.contains(asset.assetCode.issuer),
                tx,
                MintingMissingIssuersSignature
              )
            case _ => tx.validNec[SyntacticValidationFailure]
          }
          .foldLeft(tx.validNec[SyntacticValidationFailure]) { case (acc, v) => acc.combine(v) }
      case tx =>
        tx.validNec[SyntacticValidationFailure]
    }

  private[transaction] def attestationValidation(tx: TransferTransaction[T, P])(implicit
    networkPrefix:                                   NetworkPrefix
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    propositionSatisfiedValidation(tx)
      .combine(propositionEvidenceMatchValidation(tx))
      .combine(mintingIssuersValidation(tx))

  private[transaction] def inputOutputBoxesUniqueValidation(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    Validated.condNec(
      tx.newBoxes.size == tx.newBoxes.toSet.size &&
      tx.boxIdsToOpen.size == tx.boxIdsToOpen.toSet.size &&
      tx.newBoxes.map(_.id).toSet.intersect(tx.boxIdsToOpen.toSet).isEmpty,
      tx,
      InputOutputBoxesNotUnique
    )

  private[transaction] def feeValidation(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
    Validated.condNec(tx.fee >= 0L, tx, NegativeFeeFailure)

  private[transaction] def timestampValidation(
    tx: TransferTransaction[T, P]
  ): ValidatedNec[SyntacticValidationFailure, TransferTransaction[T, P]] =
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

trait SemanticallyValidatable[T] {

  def semanticValidation(t: T, boxReader: BoxReader[ProgramId, Address])(implicit
    networkPrefix:          NetworkPrefix
  ): ValidatedNec[SemanticValidationFailure, T]

}

object SemanticallyValidatable {

  trait Ops[A] {
    def typeClassInstance: SemanticallyValidatable[A]
    def self: A

    def semanticValidation(boxReader: BoxReader[ProgramId, Address])(implicit
      networkPrefix:                  NetworkPrefix
    ): ValidatedNec[SemanticValidationFailure, A] =
      typeClassInstance.semanticValidation(self, boxReader)
  }

  trait ToSemanticallyValidatableOps {

    implicit def toSemanticallyValidatable[T: SemanticallyValidatable](target: T): Ops[T] =
      new Ops[T] {
        val self = target
        val typeClassInstance = implicitly[SemanticallyValidatable[T]]
      }
  }

  trait Instances {

    implicit def polyTransferTransactionSemanticallyValidatable[P <: Proposition]
      : SemanticallyValidatable[PolyTransfer[P]] = {
      val delegate =
        new TransferTransactionSemanticallyValidatable[SimpleValue, P]

      new SemanticallyValidatable[PolyTransfer[P]] {
        override def semanticValidation(t: PolyTransfer[P], boxReader: BoxReader[ProgramId, Address])(implicit
          networkPrefix:                   NetworkPrefix
        ): ValidatedNec[SemanticValidationFailure, PolyTransfer[P]] =
          delegate.semanticValidation(t, boxReader).map(_ => t)
      }
    }

    implicit def arbitTransferTransactionSemanticallyValidatable[P <: Proposition]
      : SemanticallyValidatable[ArbitTransfer[P]] = {
      val delegate =
        new TransferTransactionSemanticallyValidatable[SimpleValue, P]

      new SemanticallyValidatable[ArbitTransfer[P]] {
        override def semanticValidation(t: ArbitTransfer[P], boxReader: BoxReader[ProgramId, Address])(implicit
          networkPrefix:                   NetworkPrefix
        ): ValidatedNec[SemanticValidationFailure, ArbitTransfer[P]] =
          delegate.semanticValidation(t, boxReader).map(_ => t)
      }
    }

    implicit def assetTransferTransactionSemanticallyValidatable[P <: Proposition]
      : SemanticallyValidatable[AssetTransfer[P]] = {
      val delegate =
        new TransferTransactionSemanticallyValidatable[TokenValueHolder, P]

      new SemanticallyValidatable[AssetTransfer[P]] {
        override def semanticValidation(t: AssetTransfer[P], boxReader: BoxReader[ProgramId, Address])(implicit
          networkPrefix:                   NetworkPrefix
        ): ValidatedNec[SemanticValidationFailure, AssetTransfer[P]] =
          delegate.semanticValidation(t, boxReader).map(_ => t)
      }
    }

    implicit def transferTransactionSemanticallyValidatable[T <: TokenValueHolder, P <: Proposition]
      : TransferTransactionSemanticallyValidatable[T, P] =
      new TransferTransactionSemanticallyValidatable[T, P]

    implicit def transactionSemanticallyValidatable[T, P <: Proposition]: SemanticallyValidatable[Transaction[T, P]] =
      new SemanticallyValidatable[Transaction[T, P]] {

        override def semanticValidation(
          t:                      Transaction[T, P],
          boxReader:              BoxReader[ProgramId, Address]
        )(implicit networkPrefix: NetworkPrefix): ValidatedNec[SemanticValidationFailure, Transaction[T, P]] =
          t match {
            case transaction: TransferTransaction[TokenValueHolder, P] =>
              transferTransactionSemanticallyValidatable[TokenValueHolder, P]
                .semanticValidation(transaction, boxReader)
                .map(_ => t)
            case t => t.validNec
          }
      }
  }
}

class TransferTransactionSemanticallyValidatable[T <: TokenValueHolder, P <: Proposition]
    extends SemanticallyValidatable[TransferTransaction[T, P]] {
  import SemanticValidation._
  import implicits._

  /**
   * Checks the stateful validity of a transaction
   *
   * @param boxReader the state to check the validity against
   * @return a success or failure denoting the result of this check
   */
  def semanticValidation(tx: TransferTransaction[T, P], boxReader: BoxReader[ProgramId, Address])(implicit
    networkPrefix:           NetworkPrefix
  ): ValidatedNec[SemanticValidationFailure, TransferTransaction[T, P]] = {
    import tx._
    // compute transaction values used for validation
    val txOutput: Int128 = tx.newBoxes.map(_.value.quantity).sum
    val unlockers = BoxUnlocker.generate(tx.from, tx.attestation)

    val inputBoxes: List[(BoxUnlocker[P, Proof[P]], Option[Box[_]])] =
      unlockers.map(u => u -> boxReader.getBox(u.closedBoxId)).toList

    val sumOfPolyInputs: Int128 = inputBoxes.collect { case (_, Some(PolyBox(_, _, value))) =>
      value.quantity
    }.sum

    syntacticSemanticValidation(tx)
      .andThen(txSpecificValidation(_)(txOutput, sumOfPolyInputs))
      .andThen(accessibleFundsValidation(_)(inputBoxes, txOutput, sumOfPolyInputs))
  }

  private[transaction] def syntacticSemanticValidation(tx: TransferTransaction[T, P])(implicit
    networkPrefix:                                         NetworkPrefix
  ): ValidatedNec[SemanticValidationFailure, TransferTransaction[T, P]] =
    tx.syntacticValidation.toEither.leftMap(SyntacticSemanticValidationFailure).toValidatedNec

  private[transaction] def txSpecificValidation(tx: TransferTransaction[T, P])(
    txOutput:                                       Int128,
    sumOfPolyInputs:                                Int128
  ): ValidatedNec[SemanticValidationFailure, TransferTransaction[T, P]] =
    tx match {
      case _: PolyTransfer[_] if tx.minting =>
        // Poly block rewards (skip enfocring)
        tx.validNec[SemanticValidationFailure]
      case _: ArbitTransfer[_] if tx.minting =>
        // Arbit block rewards (skip enforcing)
        tx.validNec[SemanticValidationFailure]
      case _: PolyTransfer[_] =>
        Validated.condNec(
          sumOfPolyInputs - tx.fee == txOutput,
          tx,
          InputOutputUnequal(sumOfPolyInputs, txOutput, tx.fee, tx.minting)
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
        Validated.condNec(
          sumOfPolyInputs - tx.fee == tx.feeChangeOutput.value.quantity,
          tx,
          InputFeeChangeOutputUnequalNonMinting(tx.feeChangeOutput.value.quantity, sumOfPolyInputs, tx.fee)
        )
    }

  private[transaction] def accessibleFundsValidation(tx: TransferTransaction[T, P])(
    inputBoxes:                                          List[(BoxUnlocker[P, Proof[P]], Option[Box[_]])],
    txOutput:                                            Int128,
    sumOfPolyInputs:                                     Int128
  ): ValidatedNec[SemanticValidationFailure, TransferTransaction[T, P]] =
    takeWhileInclusive(
      inputBoxes.toStream
        .map {
          case (unlocker, Some(box: TokenBox[_])) if unlocker.boxKey.isValid(unlocker.proposition, tx.messageToSign) =>
            Right(box.value.quantity)
          case (_, Some(box: TokenBox[TokenValueHolder @unchecked])) =>
            Left(InvalidUnlocker(box))
          case (_, Some(box)) =>
            Left(InvalidBoxType(box))
          case _ =>
            Left(BoxNotFound)
        }
    )(_.isRight)
      .foldLeft(Right(0): Either[SemanticValidationFailure, Int128]) { case (a, b) =>
        a.flatMap(i => b.map(i1 => i + i1))
      }
      .flatMap {
        // a normal transfer will fall in this case
        case sum if txOutput == sum - tx.fee =>
          Right(tx)

        // a minting transaction (of either Arbit, Polys, or Assets) will fall in this case
        case _ if tx.minting =>
          Right(tx)

        case sum if !tx.minting && txOutput != sum - tx.fee =>
          Left(
            InputOutputUnequal(
              sumOfPolyInputs,
              txOutput,
              tx.fee,
              tx.minting
            )
          )
      }
      .toValidatedNec
}

object SemanticValidation {

  /**
   * Similar to Iterable#takeWhile, but the first item that fails the condition will be kept instead of discarded
   */
  def takeWhileInclusive[A](items: Stream[A])(cond: A => Boolean): Stream[A] = {
    var foundFailure: Boolean = false
    items.takeWhile { t =>
      if (foundFailure) false
      else {
        foundFailure = !cond(t)
        true
      }
    }
  }

}

sealed abstract class SemanticValidationFailure

case class SyntacticSemanticValidationFailure(syntacticValidationFailures: NonEmptyChain[SyntacticValidationFailure])
    extends SemanticValidationFailure

case class InputOutputUnequal(sumOfPolyInputs: Int128, txOutput: Int128, fee: Int128, minting: Boolean)
    extends SemanticValidationFailure

case class InputFeeChangeOutputUnequalNonMinting(feeChangeQuantity: Int128, sumOfPolyInputs: Int128, fee: Int128)
    extends SemanticValidationFailure
case class InvalidUnlocker(box: TokenBox[TokenValueHolder]) extends SemanticValidationFailure
case class InvalidBoxType(box: Box[_]) extends SemanticValidationFailure
case object BoxNotFound extends SemanticValidationFailure
