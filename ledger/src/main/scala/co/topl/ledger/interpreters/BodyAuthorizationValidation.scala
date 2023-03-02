package co.topl.ledger.interpreters

import cats.data.NonEmptyChain
import cats.data.ValidatedNec
import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.models.Datum
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.node.models.BlockBody
import co.topl.quivr.runtime.DynamicContext

object BodyAuthorizationValidation {

  def make[F[_]: Sync](
    fetchTransaction:                   Identifier.IoTransaction32 => F[IoTransaction],
    transactionAuthorizationValidation: TransactionAuthorizationVerifier[F]
  ): F[BodyAuthorizationValidationAlgebra[F]] =
    Sync[F].delay {
      new BodyAuthorizationValidationAlgebra[F] {

        /**
         * Perform authorization validation on each of the transactions in this block
         */
        def validate(
          parentBlockId: BlockId
        )(body: BlockBody): F[ValidatedNec[BodyAuthorizationError, BlockBody]] =
          body.transactionIds
            .foldMapM(transactionId =>
              for {
                transaction <- fetchTransaction(transactionId)
                context: DynamicContext[F, String, Datum] = ???
                validationResult <- transactionAuthorizationValidation.validate(context)(transaction)
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
