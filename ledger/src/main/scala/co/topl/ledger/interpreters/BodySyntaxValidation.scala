package co.topl.ledger.interpreters

import cats.data.NonEmptySet
import cats.data.ValidatedNec
import cats.effect.Sync
import cats.implicits._
import cats.Foldable
import cats.Order
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString

import scala.collection.immutable.SortedSet

object BodySyntaxValidation {

  implicit private val orderBoxId: Order[TransactionOutputAddress] = {
    implicit val orderTypedIdentifier: Order[TransactionId] =
      Order.by[TransactionId, ByteString](_.value)(
        Order.from(ByteString.unsignedLexicographicalComparator().compare)
      )
    Order.whenEqual(
      Order.by(_.id),
      Order.by(_.index)
    )
  }

  def make[F[_]: Sync](
    fetchTransaction:               TransactionId => F[IoTransaction],
    transactionSyntacticValidation: TransactionSyntaxVerifier[F]
  ): F[BodySyntaxValidationAlgebra[F]] =
    Sync[F].delay {
      new BodySyntaxValidationAlgebra[F] {

        /**
         * Syntactically validates each of the transactions in the given block.
         */
        def validate(body: BlockBody): F[ValidatedNec[BodySyntaxError, BlockBody]] =
          for {
            transactions            <- body.transactionIds.toList.traverse(fetchTransaction)
            validatedDistinctInputs <- validateDistinctInputs(transactions).pure[F]
            validatedTransactions   <- transactions.foldMapM(validateTransaction)
          } yield validatedTransactions.combine(validatedDistinctInputs).as(body)

        /**
         * Ensure that no two transaction inputs within the block spend the same output
         */
        private def validateDistinctInputs[G[_]: Foldable](
          transactions: G[IoTransaction]
        ): ValidatedNec[BodySyntaxError, Unit] =
          NonEmptySet
            .fromSet(
              SortedSet.from(
                transactions
                  .foldMap(_.inputs.map(_.address))
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
        private def validateTransaction(transaction: IoTransaction): F[ValidatedNec[BodySyntaxError, Unit]] =
          transactionSyntacticValidation
            .validate(transaction)
            .map(
              _.void
                .leftMap(BodySyntaxErrors.TransactionSyntaxErrors(transaction, _))
                .toValidatedNec
            )
      }
    }
}
