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
        def validate(context: TypedIdentifier)(t: BlockBodyV2): F[ValidatedNec[BodySemanticError, BlockBodyV2]] =
          t.foldLeftM(AugmentedBoxState.StateAugmentation.Empty.validNec[BodySemanticError]) {
            case (Validated.Valid(augmentation), transactionId) =>
              for {
                _boxState             <- AugmentedBoxState.make(boxState)(augmentation)
                transactionValidation <- makeTransactionSemanticValidation(_boxState)
                transaction           <- fetchTransaction(transactionId)
                validationResult      <- transactionValidation.validate(context)(transaction)
              } yield validationResult
                .as(augmentation.augment(transaction))
                .leftMap(errors => NonEmptyChain(BodySemanticErrors.TransactionSemanticErrors(transaction, errors)))
            case (invalid, _) => invalid.pure[F]
          }.map(_.as(t))
      }
    }

}
