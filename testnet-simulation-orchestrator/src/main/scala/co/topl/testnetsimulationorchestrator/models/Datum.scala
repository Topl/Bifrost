package co.topl.testnetsimulationorchestrator.models

import co.topl.models._

case class AdoptionDatum(blockId: TypedIdentifier, timestamp: Timestamp)
case class BlockDatum(headerV2: BlockHeaderV2, bodyV2: BlockBodyV2)
case class TransactionDatum(transaction: Transaction)
