package co.topl.testnetsimulationorchestrator.models

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody
import co.topl.models.Timestamp

case class AdoptionDatum(blockId: BlockId, timestamp: Timestamp)
case class BlockDatum(header: BlockHeader, body: BlockBody)
case class TransactionDatum(transaction: IoTransaction)
