package co.topl.testnetsimulationorchestrator.models

import co.topl.models._
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader} // TODO remove rename, after remove models

case class AdoptionDatum(blockId: TypedIdentifier, timestamp: Timestamp)
case class BlockDatum(header: ConsensusBlockHeader, body: BlockBody)
case class TransactionDatum(transaction: Transaction)
