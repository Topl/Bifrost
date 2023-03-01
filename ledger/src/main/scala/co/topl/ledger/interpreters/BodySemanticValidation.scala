package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.utility._
import co.topl.{models => legacyModels}
import legacyModels.{Transaction, TypedIdentifier}
import co.topl.node.models.BlockBody

object BodySemanticValidation {

  def make[F[_]: Sync](
    fetchTransaction:              TypedIdentifier => F[Transaction],
    transactionSemanticValidation: TransactionSemanticValidationAlgebra[F]
  ): F[BodySemanticValidationAlgebra[F]] =
    Sync[F].delay {
      new BodySemanticValidationAlgebra[F] {

        /**
         * Semantically validates each of the transactions in the given block.  The given transactions _may_ spend
         * the outputs of previous transactions in the block, but no two transactions may spend the same input.
         */
        def validate(context: BodyValidationContext)(body: BlockBody): F[ValidatedNec[BodySemanticError, BlockBody]] =
          body.transactionIds
            .map(t => t: TypedIdentifier)
            .toList
            .foldLeftM(Chain.empty[Transaction].validNec[BodySemanticError]) {
              case (Validated.Valid(prefix), transactionId) =>
                validateTransaction(context, prefix)(transactionId).map(_.map(prefix.append))
              case (invalid, _) => invalid.pure[F]
            }
            .map(_.as(body))

        /**
         * Fetch the given transaction ID and (semantically) validate it in the context of the given block ID.
         * Transaction semantic validation uses the given augmented box state during validation.
         *
         * @return a ValidatedNec containing either errors or the original Transaction
         */
        private def validateTransaction(
          context: BodyValidationContext,
          prefix:  Chain[Transaction]
        )(transactionId: TypedIdentifier) =
          for {
            transaction <- fetchTransaction(transactionId)
            transactionValidationContext = StaticTransactionValidationContext(
              context.parentHeaderId,
              prefix,
              context.height,
              context.slot
            )
            validationResult <- transactionSemanticValidation.validate(transactionValidationContext)(transaction)
          } yield validationResult
            .leftMap(errors =>
              NonEmptyChain[BodySemanticError](BodySemanticErrors.TransactionSemanticErrors(transaction, errors))
            )
      }
    }

}
