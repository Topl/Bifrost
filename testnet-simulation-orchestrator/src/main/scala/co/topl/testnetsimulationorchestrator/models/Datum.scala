package co.topl.testnetsimulationorchestrator.models

import co.topl.{models => legacyModels}
import legacyModels._
import co.topl.consensus.models.BlockHeader
import co.topl.node.models.BlockBody

case class AdoptionDatum(blockId: TypedIdentifier, timestamp: Timestamp)
case class BlockDatum(header: BlockHeader, body: BlockBody)
case class TransactionDatum(transaction: Transaction) // TODO old Transaction Model
