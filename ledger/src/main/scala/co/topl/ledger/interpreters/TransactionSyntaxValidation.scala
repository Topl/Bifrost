package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.{Box, Proof, Proofs, Proposition, Propositions, Transaction}
import co.topl.typeclasses.implicits._

object TransactionSyntaxValidation {

  def make[F[_]: Sync]: F[TransactionSyntaxValidationAlgebra[F]] =
    Sync[F].delay(
      (
        transaction => Sync[F].delay(validators.foldMap(_.apply(transaction)).as(transaction))
      ): TransactionSyntaxValidationAlgebra[F]
    )

  private[interpreters] val validators: Chain[Transaction => ValidatedNec[TransactionSyntaxError, Unit]] =
    Chain(
      nonEmptyInputsValidation,
      distinctInputsValidation,
      maximumOutputsCountValidation,
      positiveTimestampValidation,
      positiveOutputValuesValidation,
      sufficientFundsValidation,
      proofTypeValidation
    )

  /**
   * Verify that this transaction contains at least one input
   */
  private[interpreters] def nonEmptyInputsValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.inputs.nonEmpty, (), TransactionSyntaxErrors.EmptyInputs)

  /**
   * Verify that this transaction does not spend the same box more than once
   */
  private[interpreters] def distinctInputsValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain
      .fromSeq(
        transaction.inputs.toIterable
          .groupBy(_.boxId)
          .collect {
            case (boxId, inputs) if inputs.size > 1 => TransactionSyntaxErrors.DuplicateInput(boxId)
          }
          .toSeq
      )
      .fold(().validNec[TransactionSyntaxError])(_.invalid[Unit])

  /**
   * Verify that this transaction does not contain too many outputs.  A transaction's outputs are referenced by index,
   * but that index must be a Short value.
   */
  private[interpreters] def maximumOutputsCountValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.outputs.size < Short.MaxValue, (), TransactionSyntaxErrors.ExcessiveOutputsCount)

  /**
   * Verify that the timestamp of the transaction is positive (greater than 0).  Transactions _can_ be created
   * in the past.
   */
  private[interpreters] def positiveTimestampValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.timestamp >= 0, (), TransactionSyntaxErrors.InvalidTimestamp(transaction.timestamp))

  /**
   * Verify that each transaction output contains a positive quantity (where applicable)
   */
  private[interpreters] def positiveOutputValuesValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.outputs
      .foldMap(output =>
        (output.value match {
          case t: Box.Values.Poly    => t.quantity.data.some
          case t: Box.Values.Arbit   => t.quantity.data.some
          case t: Box.Values.AssetV1 => t.quantity.data.some
          case _                     => none
        }).foldMap(quantity =>
          Validated
            .condNec(
              quantity > BigInt(0),
              (),
              TransactionSyntaxErrors.NonPositiveOutputValue(output.value): TransactionSyntaxError
            )
        )
      )

  /**
   * Ensure the input value quantities exceed or equal the (non-minting) output value quantities
   */
  private[interpreters] def sufficientFundsValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    quantityBasedValidation(transaction) { f =>
      val filteredInputs = transaction.inputs.map(_.value).filter(f.isDefinedAt)
      val filteredOutputs = transaction.outputs.filterNot(_.minting).map(_.value).filter(f.isDefinedAt)
      val inputsSum = filteredInputs.map(f).sumAll
      val outputsSum = filteredOutputs.map(f).sumAll
      Validated.condNec(
        inputsSum >= outputsSum,
        (),
        TransactionSyntaxErrors.InsufficientInputFunds(filteredInputs, filteredOutputs): TransactionSyntaxError
      )
    }

  /**
   * Validates that the proofs associated with each proposition matches the expected _type_
   *
   * (i.e. a Curve25519 Proof that is associated with an Ed25519 Proposition, this validation will fail)
   */
  private[interpreters] def proofTypeValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.inputs
      .map(input => proofTypeValidationRecursive(input.proposition, input.proof, allowUndefined = false))
      .combineAll

  /**
   * Perform validation based on the quantities of boxes grouped by type
   * @param f an extractor function which retrieves a BigInt from a Box.Value
   */
  private[interpreters] def quantityBasedValidation(transaction: Transaction)(
    f: PartialFunction[Box.Value, BigInt] => ValidatedNec[TransactionSyntaxError, Unit]
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain(
      // Extract all Poly values and their quantities
      f { case Box.Values.Poly(quantity) => quantity.data },
      // Extract all Asset values and their quantities
      f { case Box.Values.Arbit(quantity) => quantity.data }
    ).appendChain(
      // Extract all Asset values (grouped by asset code) and their quantities
      Chain.fromSeq(
        (transaction.inputs.map(_.value) ++ transaction.outputs.map(_.value))
          .collect { case a: Box.Values.AssetV1 =>
            a.assetCode
          }
          .toList
          .distinct
          .map(code => f { case a: Box.Values.AssetV1 if a.assetCode === code => a.quantity.data })
      )
    ).combineAll

  /**
   * Validate that the type of Proof _can_ satisfy the given Proposition
   * @param allowUndefined flag indicating if `Proofs.Undefined` is an acceptable proof/proof type
   *                       This is necessary to support partial compositional proofs like `Or(proofA, undefined)`
   * @return
   */
  private def proofTypeValidationRecursive(
    proposition:    Proposition,
    proof:          Proof,
    allowUndefined: Boolean
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    proposition match {
      case Propositions.PermanentlyLocked =>
        matchProof(proposition, proof)(allowUndefined = true) { case Proofs.Undefined => }
      case _: Propositions.Knowledge.Curve25519 =>
        matchProof(proposition, proof)(allowUndefined) { case _: Proofs.Knowledge.Curve25519 => }
      case _: Propositions.Knowledge.Ed25519 =>
        matchProof(proposition, proof)(allowUndefined) { case _: Proofs.Knowledge.Ed25519 => }
      case _: Propositions.Knowledge.ExtendedEd25519 =>
        matchProof(proposition, proof)(allowUndefined) { case _: Proofs.Knowledge.Ed25519 => }
      case _: Propositions.Knowledge.HashLock =>
        matchProof(proposition, proof)(allowUndefined) { case _: Proofs.Knowledge.HashLock => }
      case Propositions.Compositional.And(aProp, bProp) =>
        proof match {
          case Proofs.Undefined if allowUndefined => ().validNec[TransactionSyntaxError]
          case Proofs.Compositional.And(a, b) =>
            proofTypeValidationRecursive(aProp, a, allowUndefined = false)
              .combine(proofTypeValidationRecursive(bProp, b, allowUndefined = false))
          case _ =>
            TransactionSyntaxErrors.InvalidProofType(proposition, proof).invalidNec[Unit]
        }
      case Propositions.Compositional.Or(aProp, bProp) =>
        proof match {
          case Proofs.Undefined if allowUndefined => ().validNec[TransactionSyntaxError]
          case Proofs.Compositional.Or(a, b) =>
            proofTypeValidationRecursive(aProp, a, allowUndefined = true)
              .combine(proofTypeValidationRecursive(bProp, b, allowUndefined = true))
          case _ =>
            TransactionSyntaxErrors.InvalidProofType(proposition, proof).invalidNec[Unit]
        }
      case Propositions.Compositional.Threshold(_, propositions) =>
        proof match {
          case Proofs.Undefined if allowUndefined => ().validNec[TransactionSyntaxError]
          case Proofs.Compositional.Threshold(proofs) =>
            Validated
              .condNec(
                propositions.size == proofs.size,
                (),
                TransactionSyntaxErrors.InvalidProofType(proposition, proof): TransactionSyntaxError
              )
              .andThen(_ =>
                propositions.toList
                  .zip(proofs)
                  .map { case (proposition, proof) =>
                    proofTypeValidationRecursive(proposition, proof, allowUndefined = true)
                  }
                  .combineAll
              )
          case _ =>
            TransactionSyntaxErrors.InvalidProofType(proposition, proof).invalidNec[Unit]
        }
      case Propositions.Compositional.Not(aProposition) =>
        proof match {
          case Proofs.Undefined if allowUndefined => ().validNec[TransactionSyntaxError]
          case Proofs.Compositional.Not(aProof) =>
            proofTypeValidationRecursive(aProposition, aProof, allowUndefined = false)
          case _ =>
            TransactionSyntaxErrors.InvalidProofType(proposition, proof).invalidNec[Unit]
        }
      case _: Propositions.Contextual.HeightLock =>
        matchProof(proposition, proof)(allowUndefined) { case _: Proofs.Contextual.HeightLock => }
      case _: Propositions.Contextual.RequiredTransactionIO =>
        matchProof(proposition, proof)(allowUndefined) { case _: Proofs.Contextual.RequiredTransactionIO => }
    }

  private def matchProof(proposition: Proposition, proof: Proof)(
    allowUndefined:                   Boolean
  )(f:                                PartialFunction[Proof, Unit]): ValidatedNec[TransactionSyntaxError, Unit] =
    if (allowUndefined && proof == Proofs.Undefined)
      ().validNec
    else
      f.andThen(_.validNec[TransactionSyntaxError])
        .applyOrElse[Proof, ValidatedNec[TransactionSyntaxError, Unit]](
          proof,
          _ => (TransactionSyntaxErrors.InvalidProofType(proposition, proof): TransactionSyntaxError).invalidNec[Unit]
        )

}
