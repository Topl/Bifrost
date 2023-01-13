package co.topl.ledger.interpreters

import cats.data.{NonEmptyChain, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.{Transaction, TypedBytes, TypedIdentifier}

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
        )(body:          co.topl.node.models.BlockBody): F[ValidatedNec[BodyAuthorizationError, co.topl.node.models.BlockBody]] =
          body.transactionIds.map(TypedBytes.ioTx32).toList
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
