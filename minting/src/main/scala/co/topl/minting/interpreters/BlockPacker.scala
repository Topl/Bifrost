package co.topl.minting.interpreters

import cats.data.EitherT
import cats.effect.std.Queue
import cats.effect.Async
import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.interpreters.QuivrContext
import co.topl.ledger.models.StaticBodyValidationContext
import co.topl.ledger.models.StaticTransactionValidationContext
import co.topl.ledger.models.TransactionValidationContext
import co.topl.minting.algebras.BlockPackerAlgebra
import co.topl.models._
import co.topl.node.models.BlockBody
import co.topl.node.models.FullBlockBody
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.SelfAwareStructuredLogger

/**
 * An extremely naive and greedy implementation of the BlockPackerAlgebra.
 */
object BlockPacker {

  def make[F[_]: Async](
    mempool:                  MempoolAlgebra[F],
    fetchTransaction:         TransactionId => F[IoTransaction],
    transactionExistsLocally: TransactionId => F[Boolean],
    validateBody:             TransactionValidationContext => F[Boolean]
  ): F[BlockPackerAlgebra[F]] =
    Sync[F].delay {

      new BlockPackerAlgebra[F] {
        implicit private val logger: SelfAwareStructuredLogger[F] =
          Slf4jLogger.getLoggerFromClass[F](BlockPacker.getClass)

        def improvePackedBlock(
          parentBlockId: BlockId,
          height:        Epoch,
          slot:          Epoch
        ): F[Iterative[F, FullBlockBody]] =
          for {
            // Read all transaction IDs from the mempool
            mempoolTransactionIds <- mempool.read(parentBlockId)
            _                     <- Logger[F].debug(show"Block packing candidates=${mempoolTransactionIds.toList}")
            // The transactions that come out of the mempool arrive in no particular order
            unsortedTransactions <- mempoolTransactionIds.toList.traverse(fetchTransaction)
            // Not all transactions in the mempool may have a complete parent graph (yet), so filter out any "orphans"
            transactionsWithLocalParents <- unsortedTransactions.traverseFilter(transaction =>
              transaction.inputs
                .map(_.address.id)
                .toList
                .distinct
                .forallM(transactionExistsLocally)
                .map(Option.when(_)(transaction))
            )
            sortedTransactions = orderTransactions(transactionsWithLocalParents)
            iterative <-
              // Enqueue all of the transactions (in no particular order, which is terrible for performance and accuracy)
              Queue
                .unbounded[F, IoTransaction]
                .flatTap(queue => sortedTransactions.traverse(queue.offer))
                .map(queue =>
                  new Iterative[F, FullBlockBody] {

                    def improve(current: FullBlockBody): F[FullBlockBody] =
                      // Dequeue the next transaction (or block forever)
                      queue.take
                        .flatMap { transaction =>
                          // Attempt to stuff that transaction into our current block
                          val fullBody = current.addTransactions(transaction)
                          // If it's valid, hooray.  If not, return the previous value
                          val transactionValidationContext =
                            StaticTransactionValidationContext(parentBlockId, fullBody.transactions, height, slot)
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
  private def orderTransactions(transactions: List[IoTransaction]): List[IoTransaction] =
    // TODO: This may introduce an attack vector in which an adversary may spam 0-timestamp Transactions
    transactions.sortBy(_.datum.event.schedule.timestamp)

  def makeBodyValidator[F[_]: Sync: Logger](
    bodySyntaxValidation:        BodySyntaxValidationAlgebra[F],
    bodySemanticValidation:      BodySemanticValidationAlgebra[F],
    bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F]
  ): TransactionValidationContext => F[Boolean] = { transactionValidationContext =>
    val proposedBody =
      BlockBody(transactionValidationContext.prefix.map(_.id).toList)
    (
      EitherT(bodySyntaxValidation.validate(proposedBody).map(_.toEither)).leftMap(_.toString) >>
      EitherT(
        bodySemanticValidation
          .validate(
            StaticBodyValidationContext(
              transactionValidationContext.parentHeaderId,
              transactionValidationContext.height,
              transactionValidationContext.slot
            )
          )(proposedBody)
          .map(_.toEither)
      ).leftMap(_.toString) >>
      EitherT(
        bodyAuthorizationValidation
          .validate(
            QuivrContext.forProposedBlock[F](transactionValidationContext.height, transactionValidationContext.slot, _)
          )(proposedBody)
          .map(_.toEither)
      ).leftMap(_.toString)
    )
      .leftSemiflatTap(error => Logger[F].info(show"Block packer candidate is invalid.  reason=$error"))
      .isRight
  }
}
