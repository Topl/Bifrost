package co.topl.blockchain

import cats.{Functor, Parallel}
import cats.implicits._
import co.topl.blockchain.interpreters.EpochDataEventSourcedState
import co.topl.consensus.interpreters.{ConsensusDataEventSourcedState, EpochBoundariesEventSourcedState}
import co.topl.consensus.models.BlockId
import co.topl.eventtree.EventSourcedState
import co.topl.interpreters.BlockHeightTree
import co.topl.ledger.interpreters.{BoxState, Mempool, RegistrationAccumulator}

case class EventSourcedStates[F[_]](
  epochData:            EventSourcedState[F, EpochDataEventSourcedState.State[F], BlockId],
  blockHeightsLocal:    EventSourcedState[F, BlockHeightTree.State[F], BlockId],
  blockHeightsP2P:      EventSourcedState[F, BlockHeightTree.State[F], BlockId],
  consensusDataLocal:   EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[F], BlockId],
  consensusDataP2P:     EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[F], BlockId],
  epochBoundariesLocal: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[F], BlockId],
  epochBoundariesP2P:   EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[F], BlockId],
  boxStateLocal:        EventSourcedState[F, BoxState.State[F], BlockId],
  boxStateP2P:          EventSourcedState[F, BoxState.State[F], BlockId],
  mempool:              EventSourcedState[F, Mempool.State[F], BlockId],
  registrationsLocal:   EventSourcedState[F, RegistrationAccumulator.State[F], BlockId],
  registrationsP2P:     EventSourcedState[F, RegistrationAccumulator.State[F], BlockId]
) {

  def updateLocalStatesTo(id: BlockId)(implicit fFunctor: Functor[F], fPar: Parallel[F]): F[Unit] =
    List(
      epochData,
      blockHeightsLocal,
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // consensusDataLocal,
      epochBoundariesLocal,
      boxStateLocal,
      mempool,
      registrationsLocal
    ).parTraverse(_.stateAt(id).void).void

  def updateAllStatesTo(id: BlockId)(implicit fFunctor: Functor[F], fPar: Parallel[F]): F[Unit] =
    updateLocalStatesTo(id) &>
    List(
      blockHeightsP2P,
      // This line is included but intentionally commented out due to the N-2 epoch nature of consensus data
      // consensusDataP2P,
      epochBoundariesP2P,
      boxStateP2P,
      registrationsP2P
    ).parTraverse(_.stateAt(id).void).void
}
