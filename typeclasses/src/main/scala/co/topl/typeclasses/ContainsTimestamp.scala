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
      case t: ArbitTransfer => t.timestamp
      case t: PolyTransfer  => t.timestamp
      case t: AssetTransfer => t.timestamp
      case _                => ???
    }
  }

  object instances extends Instances
}
