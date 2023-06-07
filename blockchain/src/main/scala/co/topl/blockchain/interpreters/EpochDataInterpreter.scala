package co.topl.blockchain.interpreters

import cats.MonadThrow
import cats.effect.{Async, Resource}
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits.ClockOps
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.blockchain.algebras.{EpochData, EpochDataAlgebra}
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.interpreters.{ConsensusDataEventSourcedState, EpochBoundariesEventSourcedState}
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.models._
import co.topl.node.models.BlockBody
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._

object EpochDataInterpreter {

  def make[F[_]: MonadThrow](
    localChain:                 LocalChainAlgebra[F],
    epochDataEventSourcedState: EventSourcedState[F, EpochDataEventSourcedState.State[F], BlockId]
  ): Resource[F, EpochDataAlgebra[F]] =
    Resource.pure((epoch: Epoch) =>
      localChain.head
        .map(_.slotId.blockId)
        .flatMap(epochDataEventSourcedState.useStateAt(_)(_.getOrRaise(epoch)))
    )
}

object EpochDataEventSourcedState {

  type State[F[_]] = Store[F, Epoch, EpochData]

  def make[F[_]: Async](
    currentBlockId:              F[BlockId],
    genesisBlockId:              BlockId,
    parentChildTree:             ParentChildTree[F, BlockId],
    currentEventChanged:         BlockId => F[Unit],
    initialState:                F[State[F]],
    clock:                       ClockAlgebra[F],
    fetchBlockHeader:            BlockId => F[BlockHeader],
    fetchBlockBody:              BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra[F],
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
      F
    ], BlockId],
    consensusDataEventSourcedState: EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
      F
    ], BlockId]
  ): Resource[F, EventSourcedState[F, State[F], BlockId]] =
    EventSourcedState.OfTree
      .make(
        initialState = initialState,
        initialEventId = currentBlockId,
        applyEvent = new ApplyBlock(
          genesisBlockId,
          clock,
          fetchBlockHeader,
          fetchBlockBody,
          fetchTransaction,
          transactionRewardCalculator,
          epochBoundaryEventSourcedState,
          consensusDataEventSourcedState
        ),
        unapplyEvent =
          new UnapplyBlock(clock, fetchBlockHeader, fetchBlockBody, fetchTransaction, transactionRewardCalculator),
        parentChildTree = parentChildTree,
        currentEventChanged
      )
      .toResource

  private class ApplyBlock[F[_]: MonadThrow](
    genesisBlockId:              BlockId,
    clock:                       ClockAlgebra[F],
    fetchBlockHeader:            BlockId => F[BlockHeader],
    fetchBlockBody:              BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra[F],
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundariesEventSourcedState.EpochBoundaries[
      F
    ], BlockId],
    consensusDataEventSourcedState: EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[
      F
    ], BlockId]
  ) extends ((State[F], BlockId) => F[State[F]]) {

    def apply(state: State[F], blockId: BlockId): F[State[F]] =
      for {
        header      <- fetchBlockHeader(blockId)
        epoch       <- clock.epochOf(header.slot)
        parentEpoch <- clock.epochOf(header.parentSlot)
        _ <-
          if (epoch != parentEpoch) epochBoundaryCrossed(state)(header, epoch)
          else epochBoundaryNotCrossed(state)(header, epoch)
      } yield state

    private def epochBoundaryCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch - 1)
        updatedPreviousEpochData = previousEpochData.copy(isComplete = true)
        _ <- state.put(epoch - 1, updatedPreviousEpochData)
        stakesBoundaryBlock <-
          if (epoch >= 2)
            epochBoundaryEventSourcedState.useStateAt(header.id)(_.getOrRaise(epoch - 2))
          else
            genesisBlockId.pure[F]
        (activeStake, inactiveStake) <-
          consensusDataEventSourcedState.useStateAt(stakesBoundaryBlock)(s =>
            (s.totalActiveStake.getOrRaise(()), s.totalInactiveStake.getOrRaise(())).tupled
          )
        newEpochBoundary <- clock.epochRange(epoch)
        newEpochDataBase = EpochData(
          epoch = epoch,
          eon = 1,
          era = 0,
          isComplete = false,
          startHeight = header.height,
          endHeight = header.height,
          startSlot = newEpochBoundary.start,
          endSlot = newEpochBoundary.end,
          transactionCount = 0,
          totalTransactionReward = 0,
          activeStake = activeStake,
          inactiveStake = inactiveStake
        )
        newEpochData <- applyTransactions(newEpochDataBase)(header)
        _            <- state.put(epoch, newEpochData)
      } yield state

    private def epochBoundaryNotCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch)
        newEpochData      <- applyTransactions(previousEpochData.copy(endHeight = header.height))(header)
        _                 <- state.put(epoch, newEpochData)
      } yield state

    private def applyTransactions(epochData: EpochData)(header: BlockHeader): F[EpochData] =
      fetchBlockBody(header.id)
        .flatMap(body =>
          if (body.transactionIds.isEmpty) epochData.pure[F]
          else
            body.transactionIds
              .foldMapM(fetchTransaction(_).flatMap(transactionRewardCalculator.rewardOf))
              .map(rewards =>
                epochData.copy(
                  transactionCount = epochData.transactionCount + body.transactionIds.length,
                  totalTransactionReward = epochData.totalTransactionReward + rewards
                )
              )
        )

  }

  private class UnapplyBlock[F[_]: MonadThrow](
    clock:                       ClockAlgebra[F],
    fetchBlockHeader:            BlockId => F[BlockHeader],
    fetchBlockBody:              BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra[F]
  ) extends ((State[F], BlockId) => F[State[F]]) {

    def apply(state: State[F], blockId: BlockId): F[State[F]] =
      for {
        header      <- fetchBlockHeader(blockId)
        epoch       <- clock.epochOf(header.slot)
        parentEpoch <- clock.epochOf(header.parentSlot)
        _ <-
          if (epoch != parentEpoch) epochBoundaryCrossed(state)(header, epoch)
          else epochBoundaryNotCrossed(state)(header, epoch)
      } yield state

    private def epochBoundaryCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch - 1)
        updatedPreviousEpochData = previousEpochData.copy(isComplete = false)
        _ <- state.put(epoch - 1, updatedPreviousEpochData)
        _ <- state.remove(epoch)
      } yield state

    private def epochBoundaryNotCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch)
        newEpochData      <- unapplyTransactions(previousEpochData.copy(endHeight = header.height))(header)
        _                 <- state.put(epoch, newEpochData)
      } yield state

    private def unapplyTransactions(epochData: EpochData)(header: BlockHeader): F[EpochData] =
      fetchBlockBody(header.id)
        .flatMap(body =>
          if (body.transactionIds.isEmpty) epochData.pure[F]
          else
            body.transactionIds.reverse
              .foldMapM(fetchTransaction(_).flatMap(transactionRewardCalculator.rewardOf))
              .map(rewards =>
                epochData.copy(
                  transactionCount = epochData.transactionCount - body.transactionIds.length,
                  totalTransactionReward = epochData.totalTransactionReward - rewards
                )
              )
        )

  }
}
