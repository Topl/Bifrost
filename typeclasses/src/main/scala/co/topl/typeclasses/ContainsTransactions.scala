package co.topl.typeclasses

import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{BlockBodyV2, BlockV1, BloomFilter, Bytes, Transaction}
import simulacrum.{op, typeclass}
import co.topl.models.utility.HasLength.instances._
import Lengths._

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
    implicit val blockBodyV2: ContainsTransactions[BlockBodyV2] = _.transactions
    implicit val blockV1: ContainsTransactions[BlockV1] = _.transactions
  }
  object Instances extends Instances
}
