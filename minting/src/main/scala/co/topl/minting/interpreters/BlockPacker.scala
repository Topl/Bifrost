package co.topl.minting.interpreters

import cats.Monad
import cats.data.EitherT
import cats.effect.std.Queue
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models.{
  StaticBodyValidationContext,
  StaticTransactionValidationContext,
  TransactionValidationContext
}
import co.topl.minting.algebras.BlockPackerAlgebra
import co.topl.models._
import co.topl.models.utility.ReplaceModelUtil
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}
import scala.collection.immutable.ListSet

/**
 * An extremely naive and greedy implementation of the BlockPackerAlgebra.
 */
object BlockPacker {

  def make[F[_]: Async](
    mempool:                  MempoolAlgebra[F],
    fetchTransaction:         TypedIdentifier => F[Transaction],
    transactionExistsLocally: TypedIdentifier => F[Boolean],
    validateBody:             TransactionValidationContext => F[Boolean]
  ): F[BlockPackerAlgebra[F]] =
    Sync[F].delay {

      new BlockPackerAlgebra[F] {
        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromClass[F](BlockPacker.getClass)

        def improvePackedBlock(
          parentBlockId: TypedIdentifier,
          height:        Epoch,
          slot:          Epoch
        ): F[Iterative[F, BlockBody.Full]] =
          for {
            // Read all transaction IDs from the mempool
            mempoolTransactionIds <- mempool.read(parentBlockId)
            _                     <- Logger[F].debug(show"Block packing candidates=${mempoolTransactionIds.toList}")
            // The transactions that come out of the mempool arrive in no particular order
            unsortedTransactions <- mempoolTransactionIds.toList.traverse(fetchTransaction)
            // Not all transactions in the mempool may have a complete parent graph (yet), so filter out any "orphans"
            transactionsWithLocalParents <- unsortedTransactions.traverseFilter(transaction =>
              transaction.inputs
                .map(_.boxId.transactionId)
                .toList
                .distinct
                .forallM(transactionExistsLocally)
                .map(Option.when(_)(transaction))
            )
            sortedTransactions = orderTransactions(transactionsWithLocalParents)
            iterative <-
              // Enqueue all of the transactions (in no particular order, which is terrible for performance and accuracy)
              Queue
                .unbounded[F, Transaction]
                .flatTap(queue => sortedTransactions.traverse(queue.offer))
                .map(queue =>
                  new Iterative[F, BlockBody.Full] {

                    def improve(current: BlockBody.Full): F[BlockBody.Full] =
                      // Dequeue the next transaction (or block forever)
                      queue.take
                        .flatMap { transaction =>
                          // Attempt to stuff that transaction into our current block
                          val fullBody = current.append(transaction)
                          // If it's valid, hooray.  If not, return the previous value
                          val transactionValidationContext =
                            StaticTransactionValidationContext(parentBlockId, fullBody, height, slot)
                          validateBody(transactionValidationContext).ifF(fullBody, current)
                        }
                        .logDuration("BlockPacker Iteration")
                  }
                )
          } yield iterative
      }
    }

  /**
   * A naive mechanism to pre-sort the Transactions that are attempted into a block.  The pre-sort attempts to
   * decrease the odds of wasting an attempt on a double-spend Transaction.
   */
  private def orderTransactions(transactions: List[Transaction]): List[Transaction] =
    // TODO: This may introduce an attack vector in which an adversary may spam 0-timestamp Transactions
    transactions.sortBy(_.schedule.creation)

  def makeBodyValidator[F[_]: Monad: Logger](
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F]
  ): TransactionValidationContext => F[Boolean] = { transactionValidationContext =>
    val proposedBody =
      ListSet.empty[TypedIdentifier] ++ transactionValidationContext.prefix.map(_.id.asTypedBytes).toList
    val proposedNodeBody = ReplaceModelUtil.nodeBlock(proposedBody)
    (
      EitherT(bodySyntaxValidation.validate(proposedNodeBody).map(_.toEither)).leftMap(_.toString) >>
      EitherT(
        bodySemanticValidation
          .validate(
            StaticBodyValidationContext(
              transactionValidationContext.parentHeaderId,
              transactionValidationContext.height,
              transactionValidationContext.slot
            )
          )(proposedNodeBody)
          .map(_.toEither)
      ).leftMap(_.toString) >>
      EitherT(
        bodyAuthorizationValidation
          .validate(transactionValidationContext.parentHeaderId)(proposedNodeBody)
          .map(_.toEither)
      ).leftMap(_.toString)
    )
      .leftSemiflatTap(error => Logger[F].debug(show"Block packer candidate is invalid.  reason=$error"))
      .isRight
  }
}
