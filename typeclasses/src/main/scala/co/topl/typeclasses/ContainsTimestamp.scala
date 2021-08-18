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

  object Instances {

    implicit val blockTimestamped: ContainsTimestamp[Block] = {
      case b: BlockV1 => b.timestamp
      case b: BlockV2 => b.timestamp
    }

    implicit val transactionTimestamped: ContainsTimestamp[Transaction] = {
      case t: ArbitTransfer => t.timestamp
      case t: PolyTransfer  => t.timestamp
      case t: AssetTransfer => t.timestamp
    }
  }
}
