package co.topl.testnetsimulationorchestrator.models

import co.topl.models._

case class AdoptionDatum(blockId: TypedIdentifier, timestamp: Timestamp)
case class BlockDatum(header: BlockHeader, body: BlockBody)
case class TransactionDatum(transaction: Transaction)
