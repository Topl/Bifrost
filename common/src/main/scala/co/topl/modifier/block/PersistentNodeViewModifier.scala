package co.topl.modifier.block

import co.topl.modifier.block.BloomFilter.BloomTopic
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.transaction.Transaction
import co.topl.modifier.{ModifierId, NodeViewModifier}
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.hash.{Blake2b256, Digest32}

trait PersistentNodeViewModifier extends NodeViewModifier {
  def parentId: ModifierId
  def version: PNVMVersion
}

object PersistentNodeViewModifier {
  type PNVMVersion = Byte
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

trait TransactionCarryingPersistentNodeViewModifier[TX <: Transaction.TX] extends PersistentNodeViewModifier {

  val transactions: Seq[TX]

  lazy val merkleTree: MerkleTree[Digest32] = MerkleTree(transactions.map(tx => LeafData @@ tx.bytes))(Blake2b256)

  lazy val bloomFilter: BloomFilter = TransactionsCarryingPersistentNodeViewModifier.createBloom(transactions)

}

object TransactionsCarryingPersistentNodeViewModifier {

  /**
   * Calculates a bloom filter based on the topics in the transactions
   * @param txs sequence of transaction to create the bloom for
   * @return a bloom filter
   */
  def createBloom(txs: Seq[Transaction.TX]): BloomFilter = {
    val topics = txs.foldLeft(Set[BloomTopic]()) { (acc, tx) =>
      acc ++ tx.bloomTopics
    }

    BloomFilter(topics)
  }
}
