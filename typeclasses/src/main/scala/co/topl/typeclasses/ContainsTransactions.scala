package co.topl.typeclasses

import co.topl.models.{BlockBodyV2, BlockV1, Transaction}
import simulacrum.{op, typeclass}

/**
 * Satisfies that T contains transactions
 */
@typeclass trait ContainsTransactions[T] {
  @op("transactions") def transactionsOf(t: T): Seq[Transaction]
}

object ContainsTransactions {

  object Instances {

    implicit val blockBodyV2: ContainsTransactions[BlockBodyV2] = _.transactions
    implicit val blockV1: ContainsTransactions[BlockV1] = _.transactions
  }
}
