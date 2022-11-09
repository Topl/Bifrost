package co.topl.typeclasses

import cats.Foldable
import cats.data.Chain
import cats.implicits._
import co.topl.codecs.bytes.tetra.TetraIdentifiableInstances._
import co.topl.codecs.bytes.typeclasses.Identifiable.ops._
import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.accumulators.merkle.MerkleTree
import co.topl.crypto.hash.Blake2b
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.implicits._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{BlockV1, BloomFilter, Bytes, Transaction}
import simulacrum.{op, typeclass}
import co.topl.typeclasses.implicits._

/**
 * Satisfies that T contains transactions
 */
@typeclass trait ContainsTransactions[T] {
  @op("transactions") def transactionsOf(t: T): Seq[Transaction]

  @op("merkleTree") def merkleTreeOf(t: T): Sized.Strict[Bytes, Lengths.`32`.type] = {
    val rootHash =
      MerkleTree[Blake2b, Digest32](transactionsOf(t).map(tx => LeafData(tx.id.asTypedBytes.allBytes.toArray))).rootHash.bytes
    Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(rootHash))
  }

  @op("bloomFilter") def bloomFilterOf(t: T): BloomFilter =
    // TODO
    Sized.strictUnsafe[Bytes, Lengths.`256`.type](Bytes(Array.fill[Byte](256)(1)))
}

object ContainsTransactions {

  trait Instances {

    implicit val transactionsContainsTransactions: ContainsTransactions[Seq[Transaction]] = identity

    implicit def transactionsFoldableContainsTransactions[G[_]: Foldable]: ContainsTransactions[G[Transaction]] =
      t => t.toIterable.toSeq

    implicit val blockV1ContainsTransactions: ContainsTransactions[BlockV1] = _.transactions
  }

  object Instances extends Instances
}
