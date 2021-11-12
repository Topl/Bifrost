package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

/**
 * Satisfies that T contains a timestamp
 */
@typeclass trait ContainsTimestamp[T] {
  @op("timestamp") def timestampOf(t: T): Timestamp
}

object ContainsTimestamp {

  trait Instances {

    implicit val blockHeaderV2ContainsTimestamp: ContainsTimestamp[BlockHeaderV2] = _.timestamp
    implicit val blockV1ContainsTimestamp: ContainsTimestamp[BlockV1] = _.timestamp

    implicit val transactionContainsTimestamp: ContainsTimestamp[Transaction] = {
      case t: Transactions.Arbit => t.timestamp
      case t: Transactions.Poly  => t.timestamp
      case t: Transactions.Asset => t.timestamp
    }
  }

  object instances extends Instances
}
