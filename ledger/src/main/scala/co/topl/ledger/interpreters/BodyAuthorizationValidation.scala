package co.topl.ledger.interpreters

import cats.data.{NonEmptyChain, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.{models => legacyModels}
import co.topl.models.utility._
import legacyModels.{Transaction, TypedIdentifier}
import co.topl.node.models.BlockBody

object BodyAuthorizationValidation {

  def make[F[_]: Sync](
    fetchTransaction:                   TypedIdentifier => F[Transaction],
    transactionAuthorizationValidation: TransactionAuthorizationValidationAlgebra[F]
  ): F[BodyAuthorizationValidationAlgebra[F]] =
    Sync[F].delay {
      new BodyAuthorizationValidationAlgebra[F] {

        /**
         * Perform authorization validation on each of the transactions in this block
         */
        def validate(
          parentBlockId: TypedIdentifier
        )(body:          BlockBody): F[ValidatedNec[BodyAuthorizationError, BlockBody]] =
          body.transactionIds
            .map(t => t: TypedIdentifier)
            .toList
            .foldMapM(transactionId =>
              for {
                transaction      <- fetchTransaction(transactionId)
                validationResult <- transactionAuthorizationValidation.validate(parentBlockId)(transaction)
              } yield validationResult
                .leftMap(errors =>
                  NonEmptyChain[BodyAuthorizationError](
                    BodyAuthorizationErrors.TransactionAuthorizationErrors(transaction, errors)
                  )
                )
                .void
            )
            .map(_.as(body))
      }
    }

}
