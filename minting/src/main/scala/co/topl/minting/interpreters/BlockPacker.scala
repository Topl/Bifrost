package co.topl.minting.interpreters

import cats.data.EitherT
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.ContextlessValidationAlgebra
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.brambl.validation.algebras.{TransactionAuthorizationVerifier, TransactionCostCalculator}
import co.topl.catsutils._
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.implicits._
import co.topl.ledger.interpreters.{QuivrContext, RegistrationAccumulator}
import co.topl.ledger.models.{MempoolGraph, TransactionSemanticError}
import co.topl.minting.algebras.BlockPackerAlgebra
import co.topl.models._
import co.topl.node.models.FullBlockBody
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.{Logger, SelfAwareStructuredLogger}

import scala.collection.immutable.ListSet
import scala.concurrent.duration._

/**
 * Implements a BlockPackerAlgebra which uses the graph-nature of the Mempool combined with a Transaction Scoring algorithm to produce an "ideal" block body.
 */
object BlockPacker {

  def make[F[_]: Async](
    mempool:                     MempoolAlgebra[F],
    boxState:                    BoxStateAlgebra[F],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra[F],
    transactionCostCalculator:   TransactionCostCalculator[F],
    blockPackerValidation:       BlockPackerValidation[F],
    registrationAccumulator:     RegistrationAccumulatorAlgebra[F],
    emptyMempoolPollPeriod:      FiniteDuration = 1.second
  ): Resource[F, BlockPackerAlgebra[F]] =
    Resource.pure(
      new Impl(
        mempool,
        boxState,
        transactionRewardCalculator,
        transactionCostCalculator,
        blockPackerValidation,
        registrationAccumulator,
        emptyMempoolPollPeriod
      )
    )

  private class Impl[F[_]: Async](
    mempool:                     MempoolAlgebra[F],
    boxState:                    BoxStateAlgebra[F],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra[F],
    transactionCostCalculator:   TransactionCostCalculator[F],
    blockPackerValidation:       BlockPackerValidation[F],
    registrationAccumulator:     RegistrationAccumulatorAlgebra[F],
    emptyMempoolPollPeriod:      FiniteDuration
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
            nextIterationFunction.get
              .flatMap {
                case Some(f) => f(current)
                case _       => initFromMempool
              }

          /**
           * Initializes the state with the current mempool and cedes
           */
          private def initFromMempool: F[FullBlockBody] =
            Async[F].defer(
              Logger[F].debug("Initializing block packing from mempool") *>
              mempool
                .read(parentBlockId)
                .flatMap(g =>
                  if (g.transactions.nonEmpty) {
                    Logger[F].debug(s"Mempool contains ${g.transactions.size} entries") *>
                    filterValidTransactions(g)
                  } else {
                    // If the mempool was empty, wait and try again
                    Logger[F].debug(s"Mempool is empty.  Retrying in $emptyMempoolPollPeriod") *>
                    Async[F].cede *>
                    Async[F].delayBy(initFromMempool, emptyMempoolPollPeriod)
                  }
                )
            )

          /**
           * Step through each transaction in the mempool, and remove any transaction sub-trees that are not (currently) authorized
           */
          private def filterValidTransactions(graph: MempoolGraph): F[FullBlockBody] =
            Logger[F].debug("Validating available transactions") >>
            graph.transactions.values.toList
              .parTraverse(tx => blockPackerValidation.transactionIsValid(tx, height, slot).tupleLeft(tx))
              .flatMap(_.foldLeftM(graph) {
                case (graph, (_, true)) =>
                  graph.pure[F]
                case (graph, (transaction, false)) =>
                  Sync[F]
                    .delay(graph.removeSubtree(transaction))
                    .flatMap { case (graph, evicted) =>
                      Logger[F]
                        .debug(
                          show"Evicting invalid transaction subgraph from mempool.  ids=${evicted.toList.map(_.id)}"
                        ) >>
                      evicted.toList.map(_.id).traverse(mempool.remove) >>
                      graph.pure[F]
                    }
              })
              .flatMap(pruneUnresolvedTransactions)

          /**
           * There may be some transactions in the mempool that have missing dependencies (meaning, the local node
           * does not know about some transactions yet).  These transactions (and all recursive dependents) should
           * be removed from the graph since they can't be applied to the chain yet.
           */
          private def pruneUnresolvedTransactions(graph: MempoolGraph): F[FullBlockBody] =
            Logger[F].debug("Discarding transactions which spend unknown/unspendable UTxOs") >>
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
              .flatMap(stripDoubleSpendUnresolved)
              .guarantee(Async[F].cede)

          /**
           * Some of the "unresolved" transactions of the mempool may be double-spends, so prune away the lower-value sub-graphs.
           */
          private def stripDoubleSpendUnresolved(graph: MempoolGraph): F[FullBlockBody] =
            Logger[F].debug("Discarding lower-value double-spend transactions") >>
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
              .guarantee(Async[F].cede)
              .flatMap(_.foldLeftM(graph)(pruneDoubleSpenders(_)(_)))
              .flatMap(stripGraph)

          /**
           * Finds the first instance of a double-spend in the given graph, and removes the double-spend with the
           * lowest subgraph score.
           *
           * If no double-spends are found, moves on to the final stage of forming a block
           */
          private def stripGraph(graph: MempoolGraph): F[FullBlockBody] =
            Async[F].cede *>
            (graph.spenders.find(_._2.exists(_._2.size > 1)) match {
              // Find the first instance of a double-spender in the graph and prune its subtree
              case Some((_, spenders)) =>
                spenders.values.toList
                  .map(_.map(_._1))
                  .foldLeftM(graph)(pruneDoubleSpenders(_)(_))
                  .flatMap(stripGraph)
              // If there are no remaining double-spenders in the graph, form a block
              case _ =>
                formBlockBody(graph)
            })

          /**
           * The final step is to take the given graph containing zero double-spends and flatten it into a linear sequence of transactions
           *
           * @param graph a graph with all double-spends removed
           * @return a FullBlockBody
           */
          private def formBlockBody(graph: MempoolGraph): F[FullBlockBody] = {
            def withDependencies(transaction: IoTransaction): F[ListSet[TransactionId]] =
              transaction.inputs
                .map(_.address.id)
                .distinct
                .flatMap(graph.transactions.get)
                .parTraverse(withDependencies)
                .map(_.foldLeft(ListSet.empty[TransactionId])(_.concat(_)))
                .map(_.incl(transaction.id))

            def go(
              accepted:                            FullBlockBody,
              queue:                               ListSet[TransactionId],
              registrationAccumulatorAugmentation: RegistrationAccumulator.Augmentation
            ): F[FullBlockBody] = Async[F].cede *> (
              queue.headOption match {
                case Some(transaction) if accepted.transactions.exists(_.id == transaction.id) =>
                  go(accepted, queue.tail, registrationAccumulatorAugmentation)

                case Some(transactionId) =>
                  val transaction = graph.transactions(transactionId)
                  RegistrationAccumulator.Augmented
                    .make[F](registrationAccumulator)(registrationAccumulatorAugmentation)
                    .use(registrationAccumulator =>
                      (transaction.outputs.flatMap(_.value.value.topl).flatMap(_.registration).map(_.address).toSet --
                      transaction.inputs.flatMap(_.value.value.topl).flatMap(_.registration).map(_.address)).toList
                        .forallM(registrationAccumulator.contains(parentBlockId)(_).map(!_))
                    )
                    .ifM(
                      Async[F]
                        .defer(withDependencies(transaction))
                        .map(ListSet.from(accepted.transactionIds) ++ _)
                        .map(_.toList.map(graph.transactions))
                        .map(FullBlockBody(_)) <*
                      nextIterationFunction.set(
                        (
                          (b: FullBlockBody) =>
                            go(
                              b,
                              graph.spenders
                                .get(transaction.id)
                                .map(
                                  _.values
                                    .flatMap(_.map(_._1))
                                    .toSet
                                )
                                .foldLeft(queue.tail)(_.concat(_)),
                              registrationAccumulatorAugmentation.augment(transaction)
                            )
                        ).some
                      ),
                      Async[F].defer(go(accepted, queue.tail, registrationAccumulatorAugmentation))
                    )
                case _ =>
                  nextIterationFunction.set(((_: FullBlockBody) => Async[F].never[FullBlockBody]).some).as(accepted)
              }
            )

            go(
              FullBlockBody.defaultInstance,
              ListSet.from(graph.unresolved.keys),
              RegistrationAccumulator.Augmentation.empty
            )
          }

          /**
           * Calculate the score (reward - cost) of the given transaction
           */
          private def transactionScore(transaction: IoTransaction): F[BigInt] =
            (
              transactionRewardCalculator.rewardsOf(transaction).map(_.lvl),
              transactionCostCalculator.costOf(transaction)
            ).parMapN(_ - _)

          /**
           * Accumulate the score of the given transaction, as well as all dependent transactions
           */
          private def subgraphScore(graph: MempoolGraph)(transaction: IoTransaction): F[BigInt] =
            Async[F].cede *>
            (
              transactionScore(transaction),
              graph
                .spenders(transaction.id)
                .toList
                .map(_._2)
                .parFoldMapA {
                  case spenders if spenders.isEmpty =>
                    BigInt(0).pure[F]
                  case spenders =>
                    spenders.toList
                      .map(_._1)
                      .map(graph.transactions)
                      .parTraverse(subgraphScore(graph))
                      .map(_.max)
                }
            ).parMapN(_ + _)

          /**
           * Steps through the given collection of spenders (of a specific transaction), and if multiple
           * dependents attempt to spend the same TxO, the dependents with the lowest subgraph score will be removed
           * from the returned graph
           */
          private def pruneDoubleSpenders(graph: MempoolGraph)(spenders: Set[TransactionId]): F[MempoolGraph] =
            Logger[F].debug("Searching for double-spend transactions") >>
            spenders.toList
              .map(graph.transactions)
              .parTraverse(tx => subgraphScore(graph)(tx).tupleLeft(tx))
              .map(_.sortBy(-_._2).map(_._1))
              .flatMap {
                case Nil => graph.pure[F]
                case _ :: doubleSpenders =>
                  Logger[F].debug(show"Ignoring double-spend transactions ${doubleSpenders.map(_.id)}") *>
                  Sync[F].delay(doubleSpenders.foldLeft(graph)(_.removeSubtree(_)._1))
              }

        }
      } yield iterative
  }

}

trait BlockPackerValidation[F[_]] {

  def transactionIsValid(transaction: IoTransaction, height: Long, slot: Slot): F[Boolean]

}

object BlockPackerValidation {

  def make[F[_]: Async](
    transactionDataValidation:          ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction],
    transactionAuthorizationValidation: TransactionAuthorizationVerifier[F]
  ): Resource[F, BlockPackerValidation[F]] = {

    implicit val logger: SelfAwareStructuredLogger[F] =
      Slf4jLogger.getLoggerFromName[F]("Bifrost.BlockPackerValidation")
    Resource.pure((transaction: IoTransaction, height: Long, slot: Slot) =>
      (
        EitherT(transactionDataValidation.validate(transaction).map(_.toEither)).leftMap(_.show) >>
        EitherT(
          transactionAuthorizationValidation.validate(QuivrContext.forProposedBlock(height, slot, transaction))(
            transaction
          )
        ).leftMap(_.show)
      )
        .leftSemiflatTap(error =>
          Logger[F].warn(show"Transaction id=${transaction.id} failed validation. reason=$error")
        )
        .isRight
    )
  }
}
