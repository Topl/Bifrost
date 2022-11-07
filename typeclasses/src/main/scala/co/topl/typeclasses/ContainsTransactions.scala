package co.topl.typeclasses

import cats.Foldable
import cats.implicits._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{BlockV1, BloomFilter, Bytes, Transaction}
import simulacrum.{op, typeclass}

/**
 * Satisfies that T contains transactions
 */
@typeclass trait ContainsTransactions[T] {
  @op("transactions") def transactionsOf(t: T): Seq[Transaction]

  @op("merkleTree") def merkleTreeOf(t: T): Sized.Strict[Bytes, Lengths.`32`.type] =
    // TODO
    Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))

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
