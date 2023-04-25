package co.topl.ledger.interpreters

import cats.data.ValidatedNec
import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.node.models.BlockBody
import co.topl.quivr.runtime.DynamicContext

object BodyAuthorizationValidation {

  def make[F[_]: Sync](
    fetchTransaction:                   TransactionId => F[IoTransaction],
    transactionAuthorizationValidation: TransactionAuthorizationVerifier[F]
  ): F[BodyAuthorizationValidationAlgebra[F]] =
    Sync[F].delay {
      new BodyAuthorizationValidationAlgebra[F] {

        /**
         * Perform authorization validation on each of the transactions in this block
         */
        def validate(
          context: IoTransaction => DynamicContext[F, String, Datum]
        )(body: BlockBody): F[ValidatedNec[BodyAuthorizationError, BlockBody]] =
          body.transactionIds
            .foldMapM(transactionId =>
              for {
                transaction <- fetchTransaction(transactionId)
                quivrContext = context(transaction)
                validationResult <- transactionAuthorizationValidation.validate(quivrContext)(transaction)
              } yield validationResult
                .leftMap(error =>
                  BodyAuthorizationErrors.TransactionAuthorizationErrors(transaction, error): BodyAuthorizationError
                )
                .toValidatedNec
                .void
            )
            .map(_.as(body))
      }
    }

}
