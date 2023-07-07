package co.topl.minting.interpreters

import cats.effect._
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.brambl.validation.algebras.TransactionCostCalculator
import co.topl.catsutils._
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.models.MempoolGraph
import co.topl.minting.algebras.BlockPackerAlgebra
import co.topl.models._
import co.topl.node.models.FullBlockBody
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.ListSet
import co.topl.brambl.validation.algebras.TransactionAuthorizationVerifier
import co.topl.ledger.interpreters.{QuivrContext, RegistrationAccumulator}
import cats.data.EitherT
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._

/**
 * Implements a BlockPackerAlgebra which uses the graph-nature of the Mempool combined with a Transaction Scoring algorithm to produce an "ideal" block body.
 */
object BlockPacker {

  def make[F[_]: Async](
    mempool:                            MempoolAlgebra[F],
    boxState:                           BoxStateAlgebra[F],
    transactionRewardCalculator:        TransactionRewardCalculatorAlgebra[F],
    transactionCostCalculator:          TransactionCostCalculator[F],
    transactionAuthorizationValidation: TransactionAuthorizationVerifier[F],
    registrationAccumulator:            RegistrationAccumulatorAlgebra[F]
  ): Resource[F, BlockPackerAlgebra[F]] =
    Resource.pure(
      new Impl(
        mempool,
        boxState,
        transactionRewardCalculator,
        transactionCostCalculator,
        transactionAuthorizationValidation,
        registrationAccumulator
      )
    )

  private class Impl[F[_]: Async](
    mempool:                            MempoolAlgebra[F],
    boxState:                           BoxStateAlgebra[F],
    transactionRewardCalculator:        TransactionRewardCalculatorAlgebra[F],
    transactionCostCalculator:          TransactionCostCalculator[F],
    transactionAuthorizationValidation: TransactionAuthorizationVerifier[F],
    registrationAccumulator:            RegistrationAccumulatorAlgebra[F]
  ) extends BlockPackerAlgebra[F] {

    implicit private val logger: SelfAwareStructuredLogger[F] =
      Slf4jLogger.getLoggerFromName[F]("Bifrost.BlockPacker")

    def improvePackedBlock(
      parentBlockId: BlockId,
      height:        Epoch,
      slot:          Epoch
    ): F[Iterative[F, FullBlockBody]] =
      for {
        // Store a `Ref` containing the "next" function to run.  Each iteration should override this Ref with a new function.
        nextIterationFunction <- Ref.of(none[FullBlockBody => F[FullBlockBody]])
        iterative = new Iterative[F, FullBlockBody] {

          def improve(current: FullBlockBody): F[FullBlockBody] =
            nextIterationFunction.get.flatMap(_.getOrElse((_: FullBlockBody) => initFromMempool)(current))

          /**
           * Initializes the state with the current mempool and cedes
           */
          private def initFromMempool: F[FullBlockBody] =
            Logger[F].debug("Initializing block packing from mempool") *>
            mempool
              .read(parentBlockId)
              .flatMap(g =>
                if (g.transactions.nonEmpty) {
                  Logger[F].debug(s"Mempool contains ${g.transactions.size} entries") *>
                  nextIterationFunction
                    .set(((_: FullBlockBody) => filterValidTransactions(g)).some)
                    .as(FullBlockBody.defaultInstance)
                } else {
                  // If the mempool was empty, wait and try again
                  Logger[F].debug(s"Mempool is empty.  Retrying in 5 seconds") *>
                  Async[F].delayBy(
                    nextIterationFunction
                      .set(((_: FullBlockBody) => initFromMempool).some)
                      .as(FullBlockBody.defaultInstance),
                    5.seconds
                  )
                }
              )

          /**
           * Step through each transaction in the mempool, and remove any transaction sub-trees that are not (currently) authorized
           */
          private def filterValidTransactions(graph: MempoolGraph): F[FullBlockBody] =
            graph.transactions.values.toList
              .foldLeftM(graph) { case (graph, transaction) =>
                if (graph.transactions.contains(transaction.id))
                  transactionIsValid(transaction)
                    .ifM(
                      graph.pure[F],
                      Sync[F]
                        .delay(graph.removeSubtree(transaction))
                        .flatMap { case (graph, evicted) =>
                          Logger[F]
                            .debug(show"Ignoring invalid transaction subgraph.  ids=${evicted.toList.map(_.id)}")
                            .as(graph)
                        }
                    )
                else
                  graph.pure[F]
              }
              .flatTap(g => nextIterationFunction.set(((_: FullBlockBody) => pruneUnresolvedTransactions(g)).some))
              .as(FullBlockBody.defaultInstance)

          /**
           * There may be some transactions in the mempool that have missing dependencies (meaning, the local node
           * does not know about some transactions yet).  These transactions (and all recursive dependents) should
           * be removed from the graph since they can't be applied to the chain yet.
           */
          private def pruneUnresolvedTransactions(graph: MempoolGraph): F[FullBlockBody] =
            graph.unresolved.toList
              .traverseFilter { case (id, indices) =>
                val transaction = graph.transactions(id)
                indices.toList
                  .map(transaction.inputs(_).address)
                  .forallM(boxState.boxExistsAt(parentBlockId))
                  .ifF(none, transaction.some)
              }
              // Remove the unresolved transactions from the mempool graph
              .flatTap {
                case Nil   => ().pure[F]
                case items => Logger[F].debug(show"Ignoring unresolved transactions ${items.map(_.id)}")
              }
              .map(_.foldLeft(graph)(_.removeSubtree(_)._1))
              .flatTap(g => nextIterationFunction.set(((_: FullBlockBody) => stripDoubleSpendUnresolved(g)).some))
              .as(FullBlockBody.defaultInstance)

          /**
           * Some of the "unresolved" transactions of the mempool may be double-spends, so prune away the lower-value sub-graphs.
           */
          private def stripDoubleSpendUnresolved(graph: MempoolGraph): F[FullBlockBody] =
            Sync[F]
              .delay(
                graph.unresolved.toList
                  .flatMap { case (id, indices) =>
                    val transaction = graph.transactions(id)
                    indices.toList
                      .map { index =>
                        val input = transaction.inputs(index)
                        (input.address.id, input.address.index)
                      }
                      .tupleRight(id)
                  }
                  .groupBy(_._1)
                  .filter(_._2.length > 1)
                  .values
                  .map(_.map(_._2).toSet)
                  .toList
              )
              .flatMap(
                _.foldLeftM(graph)(pruneDoubleSpenders(_)(_))
                  .flatTap(g => nextIterationFunction.set(((_: FullBlockBody) => stripGraph(g)).some))
                  .as(FullBlockBody.defaultInstance)
              )

          /**
           * Finds the first instance of a double-spend in the given graph, and removes the double-spend with the
           * lowest subgraph score.
           *
           * If no double-spends are found, moves on to the final stage of forming a block
           */
          private def stripGraph(graph: MempoolGraph): F[FullBlockBody] =
            graph.spenders.find(_._2.exists(_._2.size > 1)) match {
              // Find the first instance of a double-spender in the graph and prune its subtree
              case Some((_, spenders)) =>
                spenders.values.toList
                  .map(_.map(_._1))
                  .foldLeftM(graph)(pruneDoubleSpenders(_)(_))
                  .flatTap(g => nextIterationFunction.set(((_: FullBlockBody) => stripGraph(g)).some))
                  .as(FullBlockBody.defaultInstance)
              // If there are no remaining double-spenders in the graph, form a block
              case _ =>
                formBlockBody(graph)
            }

          /**
           * The final step is to take the given graph containing zero double-spends and flatten it into a linear sequence of transactions
           *
           * @param graph a graph with all double-spends removed
           * @return a FullBlockBody
           */
          private def formBlockBody(graph: MempoolGraph): F[FullBlockBody] = {
            def withDependencies(transaction: IoTransaction): ListSet[IoTransaction] =
              ListSet
                .empty[IoTransaction]
                .concat(
                  transaction.inputs
                    .map(_.address.id)
                    .flatMap(graph.transactions.get)
                    .flatMap(withDependencies)
                )
                .incl(transaction)

            def go(
              accepted:                            ListSet[IoTransaction],
              queue:                               ListSet[IoTransaction],
              registrationAccumulatorAugmentation: RegistrationAccumulator.Augmentation
            ): F[ListSet[IoTransaction]] =
              queue.headOption match {
                case Some(transaction) if accepted.contains(transaction) =>
                  go(accepted, queue.tail, registrationAccumulatorAugmentation)
                case Some(transaction) =>
                  RegistrationAccumulator.Augmented
                    .make[F](registrationAccumulator)(registrationAccumulatorAugmentation)
                    .use(registrationAccumulator =>
                      (transaction.outputs.flatMap(_.value.value.topl).flatMap(_.registration).map(_.address).toSet --
                      transaction.inputs.flatMap(_.value.value.topl).flatMap(_.registration).map(_.address)).toList
                        .forallM(registrationAccumulator.contains(parentBlockId)(_).map(!_))
                    )
                    .ifM(
                      Sync[F]
                        .delay(accepted ++ withDependencies(transaction))
                        .flatMap(newAccepted =>
                          graph.spenders
                            .get(transaction.id)
                            .fold(newAccepted.pure[F])(spenders =>
                              go(
                                newAccepted,
                                queue.tail.concat(
                                  spenders.values
                                    .flatMap(_.map(_._1))
                                    .toSet
                                    .flatMap(graph.transactions.get)
                                ),
                                registrationAccumulatorAugmentation.augment(transaction)
                              )
                            )
                        ),
                      go(accepted, queue.tail, registrationAccumulatorAugmentation)
                    )
                case _ =>
                  accepted.pure[F]
              }

            nextIterationFunction.set(((_: FullBlockBody) => Async[F].never[FullBlockBody]).some) *>
            Sync[F]
              .defer(
                go(
                  ListSet.empty,
                  ListSet.from(graph.unresolved.keys.map(graph.transactions)),
                  RegistrationAccumulator.Augmentation.empty
                )
              )
              .map(transactionSet => FullBlockBody(transactionSet.toList))
          }

          /**
           * Calculate the score (reward - cost) of the given transaction
           */
          private def transactionScore(transaction: IoTransaction): F[BigInt] =
            (
              transactionRewardCalculator.rewardOf(transaction),
              transactionCostCalculator.costOf(transaction)
            ).mapN(_ - _)

          /**
           * Accumulate the score of the given transaction, as well as all dependent transactions
           */
          private def subgraphScore(graph: MempoolGraph)(transaction: IoTransaction): F[BigInt] =
            (
              transactionScore(transaction),
              graph
                .spenders(transaction.id)
                .toList
                .map(_._2)
                .foldMapM(spenders =>
                  spenders.toList.map(_._1).map(graph.transactions).traverse(subgraphScore(graph)).map(_.max)
                )
            ).mapN(_ + _)

          /**
           * Steps through the given collection of spenders (of a specific transaction), and if multiple
           * dependents attempt to spend the same TxO, the dependents with the lowest subgraph score will be removed
           * from the returned graph
           */
          private def pruneDoubleSpenders(graph: MempoolGraph)(spenders: Set[TransactionId]): F[MempoolGraph] =
            spenders.toList
              .map(graph.transactions)
              .traverse(tx => subgraphScore(graph)(tx).tupleLeft(tx))
              .map(_.sortBy(-_._2).map(_._1))
              .flatMap {
                case Nil => graph.pure[F]
                case _ :: doubleSpenders =>
                  Logger[F].debug(show"Ignoring double-spend transactions ${doubleSpenders.map(_.id)}") *>
                  Sync[F].delay(doubleSpenders.foldLeft(graph)(_.removeSubtree(_)._1))
              }

          /**
           * Determines if the given transaction has proper authorization validation in the current context.
           */
          private def transactionIsValid(transaction: IoTransaction) =
            EitherT(
              transactionAuthorizationValidation.validate(QuivrContext.forProposedBlock(height, slot, transaction))(
                transaction
              )
            )
              .leftSemiflatTap(error =>
                Logger[F].warn(s"Transaction ${transaction.id} failed authorization validation: $error")
              )
              .isRight
        }
      } yield iterative
  }

}
