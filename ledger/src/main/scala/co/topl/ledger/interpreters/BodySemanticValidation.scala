package co.topl.ledger.interpreters

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.{BlockBodyV2, Transaction, TypedIdentifier}

object BodySemanticValidation {

  def make[F[_]: Sync](
    fetchTransaction:                  TypedIdentifier => F[Transaction],
    boxState:                          BoxStateAlgebra[F],
    makeTransactionSemanticValidation: BoxStateAlgebra[F] => F[TransactionSemanticValidationAlgebra[F]]
  ): F[BodySemanticValidationAlgebra[F]] =
    Sync[F].delay {
      new BodySemanticValidationAlgebra[F] {

        /**
         * Semantically validates each of the transactions in the given block.  The given transactions _may_ spend
         * the outputs of previous transactions in the block, but no two transactions may spend the same input.
         */
        def validate(
          parentBlockId: TypedIdentifier
        )(body:          BlockBodyV2): F[ValidatedNec[BodySemanticError, BlockBodyV2]] =
          body
            .foldLeftM(AugmentedBoxState.StateAugmentation.empty.validNec[BodySemanticError]) {
              case (Validated.Valid(augmentation), transactionId) =>
                validateTransaction(parentBlockId)(augmentation)(transactionId)
              case (invalid, _) => invalid.pure[F]
            }
            .map(_.as(body))

        /**
         * Fetch the given transaction ID and (semantically) validate it in the context of the given block ID.
         * Transaction semantic validation uses the given augmented box state during validation.
         *
         * @return a ValidatedNec containing either errors or a _new_ StateAugmentation
         */
        private def validateTransaction(
          blockId:      TypedIdentifier
        )(augmentation: AugmentedBoxState.StateAugmentation)(transactionId: TypedIdentifier) =
          for {
            augmentedBoxState     <- AugmentedBoxState.make(boxState)(augmentation)
            transactionValidation <- makeTransactionSemanticValidation(augmentedBoxState)
            transaction           <- fetchTransaction(transactionId)
            validationResult      <- transactionValidation.validate(blockId)(transaction)
          } yield validationResult
            .as(augmentation.augment(transaction))
            .leftMap(errors =>
              NonEmptyChain[BodySemanticError](BodySemanticErrors.TransactionSemanticErrors(transaction, errors))
            )
      }
    }

}
