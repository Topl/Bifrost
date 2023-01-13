package co.topl.testnetsimulationorchestrator.models

import co.topl.models._
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader} // TODO remove rename, after remove models
import co.topl.node.models.{BlockBody => NodeBlockBody} // TODO remove rename, after remove models

case class AdoptionDatum(blockId: TypedIdentifier, timestamp: Timestamp)
case class BlockDatum(header: ConsensusBlockHeader, body: NodeBlockBody)
case class TransactionDatum(transaction: Transaction)
