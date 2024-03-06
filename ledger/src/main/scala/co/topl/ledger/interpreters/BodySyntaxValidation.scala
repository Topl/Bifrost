package co.topl.ledger.interpreters

import cats.data.{EitherT, NonEmptySet, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import cats.{Foldable, Order, Parallel}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{TransactionId, TransactionOutputAddress}
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
                body.rewardTransactionId.traverse(fetchTransaction).flatMap(validateRewardTransaction(transactions, _))
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
          rewardTransaction: Option[IoTransaction]
        ): F[ValidatedNec[BodySyntaxError, Unit]] =
          rewardTransaction.fold(().validNec[BodySyntaxError].pure[F])(rewardTransaction =>
            if (transactions.isEmpty)
              (BodySyntaxErrors.InvalidReward(rewardTransaction): BodySyntaxError).invalidNec[Unit].pure[F]
            else
              {
                def cond(f: => Boolean) =
                  EitherT.cond[F](f, (), BodySyntaxErrors.InvalidReward(rewardTransaction))
                for {
                  _ <- cond(rewardTransaction.inputs.length == 1)
                  // Prohibit policy/statement creation
                  _ <- cond(rewardTransaction.groupPolicies.isEmpty)
                  _ <- cond(rewardTransaction.seriesPolicies.isEmpty)
                  _ <- cond(rewardTransaction.mintingStatements.isEmpty)
                  _ <- cond(rewardTransaction.mergingStatements.isEmpty)
                  _ <- cond(rewardTransaction.splittingStatements.isEmpty)
                  // Prohibit registrations in Topl rewards
                  _ <- cond(rewardTransaction.outputs.forall(_.value.value.topl.forall(_.registration.isEmpty)))
                  // Verify quantities
                  maximumReward <- EitherT.liftF(transactions.parFoldMapA(rewardCalculator.rewardsOf))
                  _             <- cond(!maximumReward.isEmpty)
                  claimedLvls = TransactionRewardCalculator.sumLvls(rewardTransaction.outputs)(_.value)
                  _ <- cond(maximumReward.lvl >= claimedLvls)
                  claimedTopls = TransactionRewardCalculator.sumTopls(rewardTransaction.outputs)(_.value)
                  _ <- cond(maximumReward.topl >= claimedTopls)
                  claimedAssets = TransactionRewardCalculator.sumAssets(rewardTransaction.outputs)(_.value)
                  _ <- claimedAssets.toList.traverse { case (id, quantity) =>
                    cond(maximumReward.assets.get(id).exists(_ >= quantity))
                  }
                } yield ()
              }.leftWiden[BodySyntaxError].toValidatedNec
          )
      }
    }
}
