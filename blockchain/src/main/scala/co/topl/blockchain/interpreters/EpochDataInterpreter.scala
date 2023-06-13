package co.topl.blockchain.interpreters

import cats.{Applicative, MonadThrow}
import cats.effect.{Async, Resource}
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits.ClockOps
import co.topl.algebras.{ClockAlgebra, Store}
import co.topl.blockchain.algebras.EpochDataAlgebra
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.common.ContainsImmutable
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.consensus.interpreters.{ConsensusDataEventSourcedState, EpochBoundariesEventSourcedState}
import co.topl.consensus.models.{BlockHeader, BlockId}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.models._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import cats.effect.kernel.Sync
import co.topl.codecs.bytes.tetra.TetraScodecCodecs
import co.topl.proto.node.{EpochData, Int128}
import com.google.protobuf.ByteString

/**
 * Invokes an EpochDataEventSourcedState implementation along the chain's canonical head to produce EpochData
 */
object EpochDataInterpreter {

  def make[F[_]: MonadThrow](
    fetchCanonicalHead:         F[BlockId],
    epochDataEventSourcedState: EventSourcedState[F, EpochDataEventSourcedState.State[F], BlockId]
  ): Resource[F, EpochDataAlgebra[F]] =
    Resource.pure((epoch: Epoch) => fetchCanonicalHead >>= (epochDataEventSourcedState.useStateAt(_)(_.get(epoch))))
}

/**
 * An Event-Sourced State implementation which tracks Epoch Data accumulations over epochs.
 */
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

  private class ApplyBlock[F[_]: Sync](
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

    /**
     * Applies a block which starts a new epoch.  Applying this block will also complete the previous epoch.
     * @param state The current state
     * @param header The new header
     * @param epoch The new epoch
     */
    private def epochBoundaryCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        // Update the previous epoch entry (unless this is the 0th epoch)
        _ <- Applicative[F].whenA(epoch > 0)(
          state
            .getOrRaise(epoch - 1)
            .map(_.copy(isComplete = true))
            .flatMap(state.put(epoch - 1, _))
        )
        // Active/Inactive Stake calculation is delayed by 2 epochs
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
        startTimestamp   <- clock.slotToTimestamps(newEpochBoundary.start).map(_.start)
        endTimestamp     <- clock.slotToTimestamps(newEpochBoundary.end).map(_.end)
        newEpochDataBase = EpochData(
          epoch = epoch,
          eon = 1, // Hardcoded for now
          era = 0, // Hardcoded for now
          isComplete = false,
          startHeight = header.height,
          endHeight = header.height,
          startSlot = newEpochBoundary.start,
          endSlot = newEpochBoundary.end,
          startTimestamp = startTimestamp,
          endTimestamp = endTimestamp,
          transactionCount = 0,
          totalTransactionReward = Int128(ByteString.copyFrom(BigInt(0).toByteArray)),
          activeStake = Int128(ByteString.copyFrom(activeStake.toByteArray)),
          inactiveStake = Int128(ByteString.copyFrom(inactiveStake.toByteArray)),
          dataBytes = TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.length
        )
        newEpochData <- applyTransactions(newEpochDataBase)(header)
        _            <- state.put(epoch, newEpochData)
      } yield state

    /**
     * Applies a block in the middle of an epoch.
     *
     * @param state  The current state
     * @param header The new header
     * @param epoch  The current epoch
     */
    private def epochBoundaryNotCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch)
        modifiedEpochData = previousEpochData.copy(
          endHeight = header.height,
          dataBytes =
            previousEpochData.dataBytes + TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.length
        )
        newEpochData <- applyTransactions(modifiedEpochData)(header)
        _            <- state.put(epoch, newEpochData)
      } yield state

    /**
     * Applies the transaction count and reward accumulation to the given EpochData
     * @param epochData the current EpochData
     * @param header the new header
     * @return a new EpochData
     */
    private def applyTransactions(epochData: EpochData)(header: BlockHeader): F[EpochData] =
      fetchBlockBody(header.id)
        .flatMap(body =>
          if (body.transactionIds.isEmpty) epochData.pure[F]
          else
            body.transactionIds.foldLeftM(epochData) { case (epochData, id) =>
              fetchTransaction(id)
                .flatMap(transaction =>
                  (
                    transactionRewardCalculator.rewardOf(transaction),
                    Sync[F].delay(
                      ContainsImmutable.instances.ioTransactionImmutable.immutableBytes(transaction).value.size()
                    )
                  )
                    .mapN((reward, size) =>
                      epochData.copy(
                        transactionCount = epochData.transactionCount + 1,
                        totalTransactionReward = Int128(
                          ByteString.copyFrom(
                            (BigInt(epochData.totalTransactionReward.value.toByteArray) + reward).toByteArray
                          )
                        ),
                        dataBytes = epochData.dataBytes + size
                      )
                    )
                )
            }
        )

  }

  private class UnapplyBlock[F[_]: Sync](
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
          if (epoch != parentEpoch) epochBoundaryCrossed(state)(epoch)
          else epochBoundaryNotCrossed(state)(header, epoch)
      } yield state

    /**
     * Unapplies a block which traversed an epoch boundary.  The block's epoch's Data is removed from state, and the
     * previous epoch is marked as non-complete.
     * @param state the current state
     * @param epoch the unapplied block's epoch
     */
    private def epochBoundaryCrossed(state: State[F])(epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch - 1)
        updatedPreviousEpochData = previousEpochData.copy(isComplete = false)
        _ <- state.put(epoch - 1, updatedPreviousEpochData)
        _ <- state.remove(epoch)
      } yield state

    /**
     * Unapplies a block in the middle of an epoch
     * @param state the current state
     * @param header the header to unapply
     * @param epoch the header's epoch
     */
    private def epochBoundaryNotCrossed(state: State[F])(header: BlockHeader, epoch: Epoch) =
      for {
        previousEpochData <- state.getOrRaise(epoch)
        modifiedEpochData = previousEpochData.copy(
          endHeight = header.height - 1,
          dataBytes =
            previousEpochData.dataBytes - TetraScodecCodecs.consensusBlockHeaderCodec.encode(header).require.length
        )
        newEpochData <- unapplyTransactions(modifiedEpochData)(header)
        _            <- state.put(epoch, newEpochData)
      } yield state

    /**
     * Unapplies the transaction count and rewards from the given block
     * @param epochData the current epoch data
     * @param header the header to unapply
     * @return a new EpochData
     */
    private def unapplyTransactions(epochData: EpochData)(header: BlockHeader): F[EpochData] =
      fetchBlockBody(header.id)
        .flatMap(body =>
          if (body.transactionIds.isEmpty) epochData.pure[F]
          else
            body.transactionIds.reverse.foldLeftM(epochData) { case (epochData, id) =>
              fetchTransaction(id)
                .flatMap(transaction =>
                  (
                    transactionRewardCalculator.rewardOf(transaction),
                    Sync[F].delay(
                      ContainsImmutable.instances.ioTransactionImmutable.immutableBytes(transaction).value.size()
                    )
                  )
                    .mapN((reward, size) =>
                      epochData.copy(
                        transactionCount = epochData.transactionCount - 1,
                        totalTransactionReward = Int128(
                          ByteString.copyFrom(
                            (BigInt(epochData.totalTransactionReward.value.toByteArray) - reward).toByteArray
                          )
                        ),
                        dataBytes = epochData.dataBytes - size
                      )
                    )
                )
            }
        )

  }
}
