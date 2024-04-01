package co.topl.ledger.models

import cats.data.NonEmptySet
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.codecs.bytes.tetra.instances._
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.models.Bytes

/**
 * @param transactions a collection of all transactions in the mempool
 * @param spenders a mapping of UTxOs to the set of STxOs that spend them.  The key is the transaction ID and the value
 *                 is a mapping of UTxO index to the set of STxO addresses that spend the UTxO
 * @param unresolved a collection of STxOs that attempt to spend unknown UTxOs.  The key is the transaction ID and the
 *                   value is the set of input indices on that transaction that are unresolved
 */
case class MempoolGraph(
  transactions:     Map[TransactionId, IoTransactionEx],
  spenders:         Map[TransactionId, Map[Int, Set[(TransactionId, Int)]]],
  unresolved:       Map[TransactionId, NonEmptySet[Int]],
  rewardCalculator: TransactionRewardCalculatorAlgebra
) {

  lazy val ioTransactions: Map[TransactionId, IoTransaction] = transactions.view.mapValues(_.tx).toMap
  lazy val meanToplFeePerKByte = transactions.values.map(_.toplFeePerKByte).sum / txSize
  val txSize: Int = transactions.size
  val memSize: Long = transactions.values.map(_.size).sum

  /**
   * Remove the given transaction from this graph.  This operation will mark any dependent transactions as unresolved.
   * @param transaction The transaction to remove
   * @return a new MempoolGraph
   */
  def removeSingle(transaction: IoTransaction): MempoolGraph =
    if (transactions.contains(transaction.id))
      MempoolGraph(
        transactions = transactions - transaction.id,
        // Step through each input of this transaction, and remove any associated spender entries from the referenced UTxOs
        spenders = transaction.inputs.zipWithIndex.foldLeft(spenders.removed(transaction.id)) {
          case (spenders, (input, index)) =>
            spenders.updatedWith(input.address.id)(
              _.map(
                _.updatedWith(input.address.index)(
                  _.map(_.excl(transaction.id -> index))
                )
              )
            )
        },
        unresolved =
          spenders.getOrElse(transaction.id, Map.empty).values.flatten.foldLeft(unresolved.removed(transaction.id)) {
            case (unresolved, (id, index)) =>
              unresolved.updatedWith(id)(_.foldLeft(NonEmptySet.one(index))(_.combine(_)).some)
          },
        rewardCalculator = rewardCalculator
      )
    else
      this

  /**
   * Remove the given transaction from this graph and all recursive transactions that depend on it.
   * @param transaction The transaction to remove
   * @return a tuple containing (new MempoolGraph, removed transactions)
   */
  def removeSubtree(transaction: IoTransactionEx): (MempoolGraph, Set[IoTransaction]) = removeSubtree(transaction.tx)

  /**
   * Remove the given transaction from this graph and all recursive transactions that depend on it.
   * @param transaction The transaction to remove
   * @return a tuple containing (new MempoolGraph, removed transactions)
   */
  def removeSubtree(transaction: IoTransaction): (MempoolGraph, Set[IoTransaction]) =
    if (transactions.contains(transaction.id)) {
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
    } else (this -> Set.empty)

  /**
   * Add the given transaction to this graph.  Updates any relevant "spender" and "unresolved" entries.
   * @param transaction the transaction to add
   * @return an updated MempoolGraph
   */
  def add(transaction: IoTransaction): MempoolGraph = add(
    IoTransactionEx(transaction, rewardCalculator.rewardsOf(transaction))
  )

  /**
   * Add the given transaction to this graph.  Updates any relevant "spender" and "unresolved" entries.
   * @param transaction the transaction to add
   * @return an updated MempoolGraph
   */
  def add(transaction: IoTransactionEx): MempoolGraph =
    if (transactions.contains(transaction.tx.id))
      this
    else {
      val spenderEntry = transactions.values
        .flatMap(tx =>
          tx.tx.inputs.zipWithIndex.collect {
            case (input, index) if input.address.id == transaction.tx.id => (input.address.index, tx.tx.id, index)
          }
        )
        .toSeq
        .groupBy(_._1)
        .view
        .mapValues(_.map(t => (t._2, t._3)).toSet)
        .toMap
      val newSpenders = transaction.tx.inputs.zipWithIndex.foldLeft(spenders) { case (spenders, (input, index)) =>
        spenders.updatedWith(input.address.id)(
          _.map(
            _.updatedWith(input.address.index)(
              _.foldLeft(Set((transaction.tx.id, index)))(_ ++ _).some
            )
          )
        )
      }
      val newUnresolved = transaction.tx.inputs.zipWithIndex.foldLeft(unresolved) { case (unresolved, (input, index)) =>
        if (transactions.contains(input.address.id)) unresolved
        else unresolved.updatedWith(transaction.tx.id)(_.foldLeft(NonEmptySet.one(index))(_ ++ _).some)
      }
      MempoolGraph(
        transactions = transactions + (transaction.tx.id -> transaction),
        spenders = newSpenders + (transaction.tx.id      -> spenderEntry),
        unresolved = spenderEntry.values.flatten.foldLeft(newUnresolved) { case (unresolved, (id, index)) =>
          unresolved.updatedWith(id)(_.map(_ - index).flatMap(NonEmptySet.fromSet))
        },
        rewardCalculator = rewardCalculator
      )
    }
}

object MempoolGraph {

  def fromTxs(
    transactions:            Map[TransactionId, IoTransaction],
    spenders:                Map[TransactionId, Map[Int, Set[(TransactionId, Int)]]],
    unresolved:              Map[TransactionId, NonEmptySet[Int]],
    rewardCalculatorAlgebra: TransactionRewardCalculatorAlgebra
  ): MempoolGraph =
    MempoolGraph(
      transactions.view.mapValues(tx => IoTransactionEx(tx, rewardCalculatorAlgebra.rewardsOf(tx))).toMap,
      spenders,
      unresolved,
      rewardCalculatorAlgebra
    )

  def empty(rewardCalculatorAlgebra: TransactionRewardCalculatorAlgebra): MempoolGraph =
    MempoolGraph(Map.empty[TransactionId, IoTransactionEx], Map.empty, Map.empty, rewardCalculatorAlgebra)
}

case class IoTransactionEx(tx: IoTransaction, rewardQuantities: RewardQuantities) {
  val asTransmittable: Bytes = Transmittable[IoTransaction].transmittableBytes(tx)
  val size: Long = asTransmittable.size()
  val toplFee: BigInt = rewardQuantities.topl
  val toplFeePerKByte: Double = 1024 * (toplFee.toDouble / size)
}
