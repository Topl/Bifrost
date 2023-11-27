package co.topl.blockchain

import co.topl.blockchain.interpreters.EpochDataEventSourcedState
import co.topl.consensus.interpreters.{ConsensusDataEventSourcedState, EpochBoundariesEventSourcedState}
import co.topl.consensus.models.BlockId
import co.topl.eventtree.EventSourcedState
import co.topl.interpreters.BlockHeightTree
import co.topl.ledger.interpreters.{BoxState, Mempool, RegistrationAccumulator}

case class EventSourcedStates[F[_]](
  epochData:       EventSourcedState[F, EpochDataEventSourcedState.State[F], BlockId],
  blockHeights:    EventSourcedState[F, BlockHeightTree.State[F], BlockId],
  consensusData:   EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[F], BlockId],
  epochBoundaries: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[F], BlockId],
  boxState:        EventSourcedState[F, BoxState.State[F], BlockId],
  mempool:         EventSourcedState[F, Mempool.State[F], BlockId],
  registrations:   EventSourcedState[F, RegistrationAccumulator.State[F], BlockId]
)
