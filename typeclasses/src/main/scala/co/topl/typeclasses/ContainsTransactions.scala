package co.topl.typeclasses

import cats.Foldable
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
import co.topl.models._
import co.topl.typeclasses.IdentityOps._
import simulacrum.{op, typeclass}

@typeclass trait ContainsTransactionIds[T] {
  @op("transactionIds") def transactionIds(t: T): Seq[TypedIdentifier]

  @op("merkleTree") def merkleTreeOf(t: T): MerkleTree[Blake2b, Digest32] =
    MerkleTree[Blake2b, Digest32](transactionIds(t).map(id => LeafData(id.allBytes.toArray)))

  @op("merkleTreeRootHash") def merkleTreeRootHashOf(t: T): TxRoot =
    Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(merkleTreeOf(t).rootHash.bytes))
}

object ContainsTransactionIds {

  trait Instances {
    implicit val typedIdentifiersAsTxIds: ContainsTransactionIds[Seq[TypedIdentifier]] = identity

    implicit val blockBody: ContainsTransactionIds[BlockBody] = body => body.toSeq

    implicit val blockNodeBody: ContainsTransactionIds[co.topl.node.models.BlockBody] = body => body.transactionIds.map(TypedBytes.ioTx32)

    implicit def containsTxToContainTxsId[G: ContainsTransactions]: ContainsTransactionIds[G] = txs =>
      implicitly[ContainsTransactions[G]].transactionsOf(txs).map(_.id.asTypedBytes)
  }

  object Instances extends Instances
}

/**
 * Satisfies that T contains transactions
 */
@typeclass trait ContainsTransactions[T] {
  @op("transactions") def transactionsOf(t: T): Seq[Transaction]

  @op("bloomFilter") def bloomFilterOf(@annotation.nowarn t: T): BloomFilter =
    // TODO
    Sized.strictUnsafe[Bytes, Lengths.`256`.type](Bytes(Array.fill[Byte](256)(1)))
}

object ContainsTransactions {

  trait Instances {

    implicit val transactionsContainsTransactions: ContainsTransactions[Seq[Transaction]] = identity

    implicit def transactionsFoldableContainsTransactions[G[_]: Foldable]: ContainsTransactions[G[Transaction]] =
      t => t.toIterable.toSeq

  }

  object Instances extends Instances
}
