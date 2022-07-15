package co.topl.consensus.interpreters

import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import cats.{Applicative, MonadThrow, Monoid}
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.consensus.algebras.ConsensusValidationStateAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.{Ratio, Sized}

object ConsensusValidationState {

  case class ConsensusData[F[_]](
    operatorStakes:   Store[F, StakingAddresses.Operator, Int128],
    totalActiveStake: Store[F, Unit, Int128],
    registrations:    Store[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
  )

  // Captures the _last_ block ID of each epoch
  type EpochBoundaries[F[_]] = Store[F, Epoch, TypedIdentifier]

  /**
   * Constructs a ConsensusValidationState using the combination of an EpochBoundary EventSourcedState and a ConsensusState
   * EventSourcecdState.  Requests for blocks at the tip of the chain will return data from 2 epochs before that block.
   */
  def make[F[_]: MonadThrow](
    fetchHeader:                    TypedIdentifier => F[BlockHeaderV2],
    epochBoundaryEventSourcedState: EventSourcedState[F, EpochBoundaries[F]],
    consensusDataEventSourcedState: EventSourcedState[F, ConsensusData[F]],
    clock:                          ClockAlgebra[F]
  ): F[ConsensusValidationStateAlgebra[F]] =
    Applicative[F].pure {
      new ConsensusValidationStateAlgebra[F] {
        def operatorRelativeStake(currentBlockId: TypedIdentifier, slot: Slot)(
          address:                                StakingAddresses.Operator
        ): F[Option[Ratio]] =
          useStateAtTargetBoundary(currentBlockId, slot)(consensusData =>
            OptionT(consensusData.operatorStakes.get(address))
              .semiflatMap(operatorStake =>
                consensusData.totalActiveStake
                  .getOrRaise(())
                  .map(totalActiveStake => Ratio(operatorStake.data, totalActiveStake.data))
              )
              .value
          )

        def operatorRegistration(currentBlockId: TypedIdentifier, slot: Slot)(
          address:                               StakingAddresses.Operator
        ): F[Option[Box.Values.Registrations.Operator]] =
          useStateAtTargetBoundary(currentBlockId, slot)(_.registrations.get(address))

        /**
         * Determines the N-2 epoch from the given block, then determines the final block ID of the N-2 epoch.  That
         * N-2 block is used in determining the `ConsensusState` to retrieve.  Once retrieved, it is applied to the
         * given `f` function
         */
        private def useStateAtTargetBoundary[Res](
          currentBlockId: TypedIdentifier,
          slot:           Slot
        )(f:              ConsensusData[F] => F[Res]): F[Res] =
          for {
            epoch <- clock.epochOf(slot)
            // Note: Special consideration is given to epochs `-2` and `-1`.  It is assumed that the epoch boundary
            // state will point to the Genesis block in those two cases.
            targetEpoch = epoch - 2
            boundaryBlockId <- epochBoundaryEventSourcedState.useStateAt(currentBlockId)(_.getOrRaise(targetEpoch))
            res             <- consensusDataEventSourcedState.useStateAt(boundaryBlockId)(f)
          } yield res
      }
    }

  /**
   * An EventSourcedState which operates on an `EpochBoundaries`.
   *
   * Applying a block depends on whether or not the block crosses an epoch boundary.  If the block crosses an epoch
   * boundary, its parent is added to the EpochBoundaries store.  If the block does not cross an epoch boundary,
   * no operation takes place.
   *
   * Unapplying a block depends on whether or not the block crosses an epoch boundary.  If the block crosses an epoch
   * boundary, its parent is removed from the EpochBoundaries store.  If the block does not cross an epoch boundary,
   * no operation takes place.
   */
  object EpochBoundariesEventSourcedState {

    def make[F[_]: Async](
      clock:           ClockAlgebra[F],
      currentBlockId:  F[TypedIdentifier],
      parentChildTree: ParentChildTree[F, TypedIdentifier],
      initialState:    F[EpochBoundaries[F]],
      fetchSlotData:   TypedIdentifier => F[SlotData]
    ): F[EventSourcedState[F, EpochBoundaries[F]]] = {
      def applyBlock(state: EpochBoundaries[F], blockId: TypedIdentifier) =
        for {
          slotData    <- fetchSlotData(blockId)
          epoch       <- clock.epochOf(slotData.slotId.slot)
          parentEpoch <- clock.epochOf(slotData.parentSlotId.slot)
          newState <- (epoch === parentEpoch)
            .pure[F]
            .ifM(
              ifTrue = state.pure[F],
              ifFalse = state.put(parentEpoch, slotData.parentSlotId.blockId).as(state)
            )
        } yield newState

      def unapplyBlock(state: EpochBoundaries[F], blockId: TypedIdentifier) =
        for {
          slotData    <- fetchSlotData(blockId)
          epoch       <- clock.epochOf(slotData.slotId.slot)
          parentEpoch <- clock.epochOf(slotData.parentSlotId.slot)
          newState <- (epoch === parentEpoch)
            .pure[F]
            .ifM(
              ifTrue = state.pure[F],
              ifFalse = state.remove(parentEpoch).as(state)
            )
        } yield newState

      EventSourcedState.OfTree.make(
        initialState = initialState,
        initialEventId = currentBlockId,
        applyEvent = applyBlock,
        unapplyEvent = unapplyBlock,
        parentChildTree = parentChildTree
      )
    }
  }

  /**
   * An EventSourcedState which operates on a `ConsensusData`.
   *
   * Applying blocks will update total and individual arbit/stake values according to the transaction.  Spent arbits
   * are subtracted from any staking parties, and output arbits are added to the corresponding staking parties.
   * Similarly, spent Operator Registrations are deregistered, and output Operator Registrations become registered.
   *
   * Unapplying blocks will do the reverse.  Output Operator Registrations become deregistered, and spent Operator Registrations
   * become re-registered.  Output arbits are subtracted from the staking parties, and spent arbits are added to the staking parties.
   */
  object ConsensusDataEventSourcedState {

    def make[F[_]: Async](
      currentBlockId:         F[TypedIdentifier],
      parentChildTree:        ParentChildTree[F, TypedIdentifier],
      initialState:           F[ConsensusData[F]],
      fetchBlockBody:         TypedIdentifier => F[BlockBodyV2],
      fetchTransaction:       TypedIdentifier => F[Transaction],
      fetchTransactionOutput: Box.Id => F[Transaction.Output]
    ): F[EventSourcedState[F, ConsensusData[F]]] =
      EventSourcedState.OfTree.make(
        initialState = initialState,
        initialEventId = currentBlockId,
        applyEvent = new ApplyBlock(fetchBlockBody, fetchTransaction, fetchTransactionOutput),
        unapplyEvent = new UnapplyBlock(fetchBlockBody, fetchTransaction, fetchTransactionOutput),
        parentChildTree = parentChildTree
      )

    private class ApplyBlock[F[_]: MonadThrow](
      fetchBlockBody:         TypedIdentifier => F[BlockBodyV2],
      fetchTransaction:       TypedIdentifier => F[Transaction],
      fetchTransactionOutput: Box.Id => F[Transaction.Output]
    ) extends ((ConsensusData[F], TypedIdentifier) => F[ConsensusData[F]]) {

      def apply(state: ConsensusData[F], blockId: TypedIdentifier): F[ConsensusData[F]] =
        for {
          body                <- fetchBlockBody(blockId)
          transactions        <- body.toList.traverse(fetchTransaction)
          stakeChanges        <- transactions.foldMapM(calculateStakeChanges)
          registrationChanges <- transactions.foldMapM(calculateRegistrationChanges)
          previousTotalStake  <- state.totalActiveStake.getOrRaise(())
          _ <- state.totalActiveStake.put((), Sized.maxUnsafe(previousTotalStake.data + stakeChanges.totalStakeChange))
          _ <- stakeChanges.deltas.toSeq.traverseTap { case (address, quantity) =>
            OptionT(state.operatorStakes.get(address))
              .fold(quantity)(_.data + quantity)
              .map[Int128](Sized.maxUnsafe)
              .flatMap(newQuantity => state.operatorStakes.put(address, newQuantity))
          }
          _ <- registrationChanges.toSeq.traverseTap {
            case (address, Some(registration)) =>
              state.registrations.put(address, registration)
            case (address, _) =>
              state.registrations.remove(address)
          }
        } yield state

      private def calculateStakeChanges(transaction: Transaction): F[StakeChanges] =
        for {
          inputStakeChanges <- transaction.inputs
            .collect { case Transaction.Input(boxId, _, _, Box.Values.Arbit(quantity)) =>
              boxId -> (quantity.data * -1)
            }
            .foldMapM { case (boxId, negativeQuantity) =>
              fetchTransactionOutput(boxId).map(output =>
                output.address.stakingAddress match {
                  case StakingAddresses.NonStaking =>
                    StakeChanges(Map.empty, negativeQuantity)
                  case s: StakingAddresses.Operator =>
                    StakeChanges(Map(s -> (negativeQuantity)), negativeQuantity)
                }
              )
            }
          outputStakeChanges = transaction.outputs.foldMap {
            case Transaction
                  .Output(FullAddress(_, _, o: StakingAddresses.Operator, _), Box.Values.Arbit(quantity), _) =>
              StakeChanges(Map(o -> quantity.data), quantity.data)
          }
        } yield inputStakeChanges.combine(outputStakeChanges)

      private def calculateRegistrationChanges(
        transaction: Transaction
      ): F[Map[StakingAddresses.Operator, Option[Box.Values.Registrations.Operator]]] =
        for {
          spentOperatorBoxIds <- transaction.inputs
            .collect { case Transaction.Input(boxId, _, _, _: Box.Values.Registrations.Operator) =>
              boxId
            }
            .pure[F]
          allStakingAddresses <- spentOperatorBoxIds.traverse(fetchTransactionOutput(_).map(_.address.stakingAddress))
          operatorAddresses = allStakingAddresses.toIterable
            .flatMap(_.some.collect { case o: StakingAddresses.Operator => o })
            .toSet
          deregistrations = operatorAddresses.toList.tupleRight(none[Box.Values.Registrations.Operator]).toMap
          registrations = transaction.outputs
            .collect {
              case Transaction.Output(
                    FullAddress(_, _, a: StakingAddresses.Operator, _),
                    o: Box.Values.Registrations.Operator,
                    _
                  ) =>
                a -> o.some
            }
            .toIterable
            .toMap
        } yield deregistrations.combine(registrations)

    }

    private class UnapplyBlock[F[_]: MonadThrow](
      fetchBlockBody:         TypedIdentifier => F[BlockBodyV2],
      fetchTransaction:       TypedIdentifier => F[Transaction],
      fetchTransactionOutput: Box.Id => F[Transaction.Output]
    ) extends ((ConsensusData[F], TypedIdentifier) => F[ConsensusData[F]]) {

      def apply(state: ConsensusData[F], blockId: TypedIdentifier): F[ConsensusData[F]] =
        for {
          body                <- fetchBlockBody(blockId)
          transactions        <- body.toList.traverse(fetchTransaction)
          stakeChanges        <- transactions.foldMapM(calculateStakeChanges)
          registrationChanges <- transactions.foldMapM(calculateRegistrationChanges)
          previousTotalStake  <- state.totalActiveStake.getOrRaise(())
          _ <- state.totalActiveStake.put((), Sized.maxUnsafe(previousTotalStake.data + stakeChanges.totalStakeChange))
          _ <- stakeChanges.deltas.toSeq.traverseTap { case (address, quantity) =>
            OptionT(state.operatorStakes.get(address))
              .fold(quantity)(_.data + quantity)
              .map[Int128](Sized.maxUnsafe)
              .flatMap(newQuantity => state.operatorStakes.put(address, newQuantity))
          }
          _ <- registrationChanges.toSeq.traverseTap {
            case (address, Some(registration)) =>
              state.registrations.put(address, registration)
            case (address, _) =>
              state.registrations.remove(address)
          }
        } yield state

      private def calculateStakeChanges(transaction: Transaction): F[StakeChanges] =
        for {
          outputStakeChanges <- transaction.outputs.reverse
            .foldMap {
              case Transaction
                    .Output(FullAddress(_, _, o: StakingAddresses.Operator, _), Box.Values.Arbit(quantity), _) =>
                val negativeQuantity = quantity.data * -1
                StakeChanges(Map(o -> negativeQuantity), negativeQuantity)
            }
            .pure[F]
          inputStakeChanges <- transaction.inputs.reverse
            .collect { case Transaction.Input(boxId, _, _, Box.Values.Arbit(quantity)) =>
              boxId -> quantity.data
            }
            .foldMapM { case (boxId, quantity) =>
              fetchTransactionOutput(boxId).map(output =>
                output.address.stakingAddress match {
                  case StakingAddresses.NonStaking =>
                    StakeChanges(Map.empty, quantity)
                  case s: StakingAddresses.Operator =>
                    StakeChanges(Map(s -> (quantity)), quantity)
                }
              )
            }
        } yield outputStakeChanges.combine(inputStakeChanges)

      private def calculateRegistrationChanges(
        transaction: Transaction
      ): F[Map[StakingAddresses.Operator, Option[Box.Values.Registrations.Operator]]] =
        for {
          registrations <- transaction.outputs.reverse
            .collect {
              case Transaction.Output(
                    FullAddress(_, _, a: StakingAddresses.Operator, _),
                    o: Box.Values.Registrations.Operator,
                    _
                  ) =>
                a -> none[Box.Values.Registrations.Operator]
            }
            .toIterable
            .toMap
            .pure[F]
          spentOperatorBoxes <- transaction.inputs.reverse
            .collect { case Transaction.Input(boxId, _, _, v: Box.Values.Registrations.Operator) =>
              boxId -> v
            }
            .pure[F]
          addressedBoxes <- spentOperatorBoxes.traverse { case (boxId, value) =>
            fetchTransactionOutput(boxId).map(_.address.stakingAddress).tupleRight(value)
          }
          deregistrations = addressedBoxes.toIterable
            .flatMap(_.some.collect { case (o: StakingAddresses.Operator, value) => o -> value.some })
            .toMap
        } yield registrations.combine(deregistrations)
    }

    private case class StakeChanges(deltas: Map[StakingAddresses.Operator, BigInt], totalStakeChange: BigInt)

    implicit private val stakeChangesMonoid: Monoid[StakeChanges] =
      Monoid.instance(
        StakeChanges(Map.empty, BigInt(0)),
        (a, b) => {
          val newDeltas =
            b.deltas.foldLeft(a.deltas) { case (deltas, (address, quantity)) =>
              deltas.updatedWith(address)(_.fold(quantity)(_ + quantity).some)
            }
          StakeChanges(newDeltas, a.totalStakeChange + b.totalStakeChange)
        }
      )

    implicit private val registrationChangesMonoid
      : Monoid[Map[StakingAddresses.Operator, Option[Box.Values.Registrations.Operator]]] =
      Monoid.instance(Map.empty, _ ++ _)
  }
}
