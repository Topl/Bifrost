package co.topl.typeclasses

import cats.Foldable
import cats.data.ValidatedNec
import cats.implicits._
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.ioTransactionAsIoTransactionOps
import co.topl.crypto.accumulators.LeafData
import co.topl.crypto.accumulators.merkle.MerkleTree
import co.topl.crypto.hash.Blake2b
import co.topl.crypto.hash.Blake2bHash
import co.topl.crypto.hash.digest.Digest
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.digest.InvalidDigestFailure
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.node.models.FullBlockBody
import com.google.protobuf.ByteString
import simulacrum.op
import simulacrum.typeclass

@typeclass trait ContainsTransactionIds[T] {
  @op("transactionIds") def transactionIds(t: T): Seq[Identifier.IoTransaction32]

  @op("merkleTree") def merkleTreeOf(t: T): MerkleTree[Blake2b, Digest32] = {
    // The current MerkleTree implementation will, by default, use a shared digest and hash instance,
    // which introduces thread-safety issues.  We need to create a new instance for each call to avoid it.
    implicit val digest: Digest[Digest32] = new Digest[Digest32] {
      override def size: Int = Digest32.size

      override def from(bytes: Array[Byte]): ValidatedNec[InvalidDigestFailure, Digest32] = Digest32.validated(bytes)

      override def bytes(d: Digest32): Array[Byte] = d.value
    }
    implicit val hash: Blake2bHash[Digest32] = new Blake2bHash[Digest32] {}
    MerkleTree[Blake2b, Digest32](transactionIds(t).map(id => LeafData(id.evidence.digest.value.toByteArray)))
  }

  @op("merkleTreeRootHash") def merkleTreeRootHashOf(t: T): TxRoot =
    Sized.strictUnsafe[Bytes, Lengths.`32`.type](ByteString.copyFrom(merkleTreeOf(t).rootHash.value))
}

object ContainsTransactionIds {

  trait Instances {

    implicit val blockNodeBody: ContainsTransactionIds[co.topl.node.models.BlockBody] = _.transactionIds

    implicit def containsTxToContainTxsId[G: ContainsTransactions]: ContainsTransactionIds[G] = txs =>
      implicitly[ContainsTransactions[G]].transactionsOf(txs).map(_.id)
  }

  object Instances extends Instances
}

/**
 * Satisfies that T contains transactions
 */
@typeclass trait ContainsTransactions[T] {
  @op("transactions") def transactionsOf(t: T): Seq[IoTransaction]

  @op("bloomFilter") def bloomFilterOf(@annotation.nowarn t: T): BloomFilter =
    // TODO
    Sized.strictUnsafe[Bytes, Lengths.`256`.type](ByteString.copyFrom(Array.fill[Byte](256)(1)))
}

object ContainsTransactions {

  trait Instances {

    implicit val fullBlockBodyContainsTransactions: ContainsTransactions[FullBlockBody] = _.transactions

    implicit val transactionsContainsTransactions: ContainsTransactions[Seq[IoTransaction]] = identity

    implicit def transactionsFoldableContainsTransactions[G[_]: Foldable]: ContainsTransactions[G[IoTransaction]] =
      t => t.toIterable.toSeq

  }

  object Instances extends Instances
}
