package co.topl.typeclasses

import co.topl.models.{Block, BlockV1, BlockV2, Transaction}
import simulacrum.{op, typeclass}

@typeclass trait ContainsTransactions[T] {
  @op("transactions") def transactionsOf(t: T): Seq[Transaction]
}

object ContainsTransactions {

  object Instances {

    implicit val blockContainsTransactions: ContainsTransactions[Block] = {
      case b: BlockV1 => b.transactions
      case b: BlockV2 => b.transactions
    }
  }
}
