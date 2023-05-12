package co.topl.ledger.interpreters

import cats.data.NonEmptySet
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._

/**
 * @param transactions a collection of all transactions in the mempool
 * @param spenders a mapping of UTxOs to the set of STxOs that spend them.  The key is the transaction ID and the value
 *                 is a mapping of UTxO index to the set of STxO addresses that spend the UTxO
 * @param unresolved a collection of STxOs that attempt to spend unknown UTxOs.  The key is the transaction ID and the
 *                   value is the set of input indices on that transaction that are unresolved
 */
case class MempoolGraph(
  transactions: Map[TransactionId, IoTransaction],
  spenders:     Map[TransactionId, Map[Int, Set[(TransactionId, Int)]]],
  unresolved:   Map[TransactionId, NonEmptySet[Int]]
) {

  /**
   * Remove the given transaction from this graph.  This operation will mark any dependent transactions as unresolved.
   * @param transaction The transaction to remove
   * @return a new MempoolGraph
   */
  def removeSingle(transaction: IoTransaction): MempoolGraph =
    MempoolGraph(
      transactions = transactions - transaction.id,
      // Step through each input of this transaction, and remove any associated spender entries from the referenced UTxOs
      spenders = transaction.inputs.foldLeft(spenders.removed(transaction.id)) { case (spenders, output) =>
        spenders.updatedWith(output.address.id)(
          _.map(
            _.updatedWith(output.address.index)(
              _.map(_.excl(transaction.id -> output.address.index))
            )
          )
        )
      },
      unresolved =
        spenders.getOrElse(transaction.id, Map.empty).values.flatten.foldLeft(unresolved.removed(transaction.id)) {
          case (unresolved, (id, index)) =>
            unresolved.updatedWith(id)(_.foldLeft(NonEmptySet.one(index))(_.combine(_)).some)
        }
    )

  /**
   * Remove the given transaction from this graph and all recursive transactions that depend on it.
   * @param transaction The transaction to remove
   * @return a tuple containing (new MempoolGraph, removed transactions)
   */
  def removeSubtree(transaction: IoTransaction): (MempoolGraph, Set[IoTransaction]) = {
    val (newGraph, removed) =
      spenders
        .getOrElse(transaction.id, Map.empty)
        .values
        .flatten
        .foldLeft(this -> Set.empty[IoTransaction]) { case ((graph, removedTransactions), (id, _)) =>
          transactions.get(id).foldLeft((graph, removedTransactions)) {
            case ((graph, removedTransactions), transaction) =>
              val (newGraph, newRemoved) = graph.removeSubtree(transaction)
              newGraph -> (removedTransactions ++ newRemoved)
          }
        }
    newGraph.removeSingle(transaction) -> (removed + transaction)
  }

  /**
   * Add the given transaction to this graph.  Updates any relevant "spender" and "unresolved" entries.
   * @param transaction the transaction to add
   * @return an updated MempoolGraph
   */
  def add(transaction: IoTransaction): MempoolGraph = {
    val spenderEntry =
      unresolved.toSeq
        .flatMap { case (id, indices) =>
          val tx = transactions(id)
          indices.toIterable
            .filter(index => tx.inputs.lift(index).exists(_.address.id == transaction.id))
            .map(index => (tx.inputs(index).address.index, (transaction.id, index)))
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).toSet)
        .toMap
    MempoolGraph(
      transactions = transactions + (transaction.id -> transaction),
      spenders = spenders + (transaction.id         -> spenderEntry),
      unresolved = spenderEntry.values.flatten.foldLeft(unresolved) { case (unresolved, (id, index)) =>
        unresolved.updatedWith(id)(_.map(_ - index).flatMap(NonEmptySet.fromSet))
      }
    )
  }
}
