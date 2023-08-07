package co.topl.ledger.interpreters

import cats.data.{EitherT, NonEmptySet, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import cats.{Foldable, Order, Parallel}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.node.models.BlockBody
import co.topl.numerics.implicits._
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

  def make[F[_]: Sync: Parallel](
    fetchTransaction:               TransactionId => F[IoTransaction],
    transactionSyntacticValidation: TransactionSyntaxVerifier[F],
    rewardCalculator:               TransactionRewardCalculatorAlgebra[F]
  ): F[BodySyntaxValidationAlgebra[F]] =
    Sync[F].delay {
      new BodySyntaxValidationAlgebra[F] {

        /**
         * Syntactically validates each of the transactions in the given block.
         */
        def validate(body: BlockBody): F[ValidatedNec[BodySyntaxError, BlockBody]] =
          body.transactionIds
            .traverse(fetchTransaction)
            .flatMap(transactions =>
              List(
                Sync[F].delay(validateDistinctInputs(transactions)),
                transactions.parFoldMapA(validateTransaction),
                body.rewardTransactionId.foldMapM(
                  fetchTransaction(_).flatMap(validateRewardTransaction(transactions, _))
                )
              ).parSequence
                .map(_.combineAll)
                .map(_.as(body))
            )

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
          EitherT(transactionSyntacticValidation.validate(transaction))
            .leftMap(BodySyntaxErrors.TransactionSyntaxErrors(transaction, _))
            .leftWiden[BodySyntaxError]
            .void
            .toValidatedNec

        /**
         * Ensure that the claimed reward transaction is valid and contains the proper reward quantity
         * @param transactions The normal transactions of the block
         * @param rewardTransaction The claimed reward
         */
        private def validateRewardTransaction[G[_]: Foldable](
          transactions:      G[IoTransaction],
          rewardTransaction: IoTransaction
        ): F[ValidatedNec[BodySyntaxError, Unit]] =
          EitherT
            .cond[F](
              rewardTransaction.inputs.length == 1 &&
              rewardTransaction.outputs.length == 1 &&
              rewardTransaction.outputs.head.value.value.isLvl,
              rewardTransaction.outputs.head.value.value.lvl.get.quantity: BigInt,
              BodySyntaxErrors.InvalidReward(rewardTransaction)
            )
            .flatMapF(definedQuantity =>
              transactions
                .parFoldMapA(rewardCalculator.rewardOf)
                .map(maxReward =>
                  Either.cond(definedQuantity <= maxReward, (), BodySyntaxErrors.InvalidReward(rewardTransaction))
                )
            )
            .leftWiden[BodySyntaxError]
            .toValidatedNec
      }
    }
}
