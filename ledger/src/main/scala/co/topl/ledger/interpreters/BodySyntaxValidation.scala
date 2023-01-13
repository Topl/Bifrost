package co.topl.ledger.interpreters

import cats.data.{NonEmptyChain, NonEmptySet, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import cats.{Foldable, Order}
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models._

import scala.collection.immutable.SortedSet

object BodySyntaxValidation {

  implicit private val orderBoxId: Order[Box.Id] = {
    implicit val orderTypedIdentifier: Order[TypedIdentifier] =
      Order.by[TypedIdentifier, Bytes](_.allBytes)(Order.from[Bytes]((a, b) => a.compare(b)))
    Order.whenEqual(
      Order.by(_.transactionId),
      Order.by(_.transactionOutputIndex)
    )
  }

  def make[F[_]: Sync](
    fetchTransaction:               TypedIdentifier => F[Transaction],
    transactionSyntacticValidation: TransactionSyntaxValidationAlgebra[F]
  ): F[BodySyntaxValidationAlgebra[F]] =
    Sync[F].delay {
      new BodySyntaxValidationAlgebra[F] {

        /**
         * Syntactically validates each of the transactions in the given block.
         */
        def validate(
          body: co.topl.node.models.BlockBody
        ): F[ValidatedNec[BodySyntaxError, co.topl.node.models.BlockBody]] =
          for {
            transactions            <- body.transactionIds.map(TypedBytes.ioTx32).toList.traverse(fetchTransaction)
            validatedDistinctInputs <- validateDistinctInputs(transactions).pure[F]
            validatedTransactions   <- transactions.foldMapM(validateTransaction)
          } yield validatedTransactions.combine(validatedDistinctInputs).as(body)

        /**
         * Ensure that no two transaction inputs within the block spend the same output
         */
        private def validateDistinctInputs[G[_]: Foldable](
          transactions: G[Transaction]
        ): ValidatedNec[BodySyntaxError, Unit] =
          NonEmptySet
            .fromSet(
              SortedSet.from(
                transactions
                  .foldMap(_.inputs.map(_.boxId))
                  .groupBy(identity)
                  .collect {
                    case (boxId, boxIds) if boxIds.size > 1 => boxId
                  }
              )
            )
            .map(BodySyntaxErrors.DoubleSpend)
            .toInvalidNec(())

        /**
         * Performs syntactic validation on the given transaction.
         */
        private def validateTransaction(transaction: Transaction): F[ValidatedNec[BodySyntaxError, Unit]] =
          transactionSyntacticValidation
            .validate(transaction)
            .map(
              _.void
                .leftMap(BodySyntaxErrors.TransactionSyntaxErrors(transaction, _))
                .leftMap(NonEmptyChain[BodySyntaxError](_))
            )
      }
    }
}
