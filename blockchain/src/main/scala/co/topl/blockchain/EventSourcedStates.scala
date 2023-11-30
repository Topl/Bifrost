package co.topl.blockchain

import cats.{Functor, NonEmptyParallel}
import cats.implicits._
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
) {

  def updateTo(id: BlockId)(implicit fFunctor: Functor[F], fNonEmptyPar: NonEmptyParallel[F]): F[Unit] =
    epochData.stateAt(id).void &>
    blockHeights.stateAt(id).void &>
    // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
    // consensusData.stateAt(id).void &>
    epochBoundaries.stateAt(id).void &>
    boxState.stateAt(id).void &>
    mempool.stateAt(id).void &>
    registrations.stateAt(id).void
}
