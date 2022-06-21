package co.topl.ledger.interpreters

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.{BlockBodyV2, Transaction, TypedIdentifier}

object BodySyntaxValidation {

  def make[F[_]: Sync](
    fetchTransaction:               TypedIdentifier => F[Transaction],
    transactionSyntacticValidation: TransactionSyntaxValidationAlgebra[F]
  ): F[BodySyntaxValidationAlgebra[F]] =
    Sync[F].delay {
      new BodySyntaxValidationAlgebra[F] {

        /**
         * Syntactically validates each of the transactions in the given block.
         */
        def validate(body: BlockBodyV2): F[ValidatedNec[BodySyntaxError, BlockBodyV2]] =
          body.foldMapM(validateTransaction).map(_.as(body))

        /**
         * Performs syntactic validation on the given transaction ID.
         */
        private def validateTransaction(
          transactionId: TypedIdentifier
        ): F[Validated[NonEmptyChain[BodySyntaxError], Unit]] =
          fetchTransaction(transactionId)
            .flatMap(transaction =>
              transactionSyntacticValidation
                .validate(transaction)
                .map(
                  _.void
                    .leftMap(BodySyntaxErrors.TransactionSyntaxErrors(transaction, _))
                    .leftMap(NonEmptyChain[BodySyntaxError](_))
                )
            )
      }
    }
}
