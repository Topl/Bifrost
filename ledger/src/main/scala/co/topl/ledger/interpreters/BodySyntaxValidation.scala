package co.topl.ledger.interpreters

import cats.data.{NonEmptyChain, ValidatedNec}
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
        def validate(t: BlockBodyV2): F[ValidatedNec[BodySyntaxError, BlockBodyV2]] =
          t.traverse(
            fetchTransaction(_)
              .flatMap(transaction =>
                transactionSyntacticValidation
                  .validate(transaction)
                  .map(
                    _.void
                      .leftMap(BodySyntaxErrors.TransactionSyntaxErrors(transaction, _))
                      .leftMap(NonEmptyChain[BodySyntaxError](_))
                  )
              )
          ).map(_.combineAll.as(t))
      }
    }
}
