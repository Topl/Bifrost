package co.topl.typeclasses

import co.topl.models.{
  ArbitTransfer,
  AssetTransfer,
  Block,
  BlockV1,
  BlockV2,
  PolyTransfer,
  TetraTransfer,
  Timestamp,
  Transaction
}
import simulacrum.{op, typeclass}

@typeclass trait Timestamped[T] {
  @op("timestamp") def timestampOf(t: T): Timestamp
}

object TimestampedInstances {

  implicit val blockTimestamped: Timestamped[Block] = {
    case b: BlockV1 => b.timestamp
    case b: BlockV2 => b.timestamp
  }

  implicit val transactionTimestamped: Timestamped[Transaction] = {
    case t: ArbitTransfer => t.timestamp
    case t: PolyTransfer  => t.timestamp
    case t: AssetTransfer => t.timestamp
    case t: TetraTransfer => t.timestamp
  }
}
