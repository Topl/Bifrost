package co.topl.consensus.interpreters

import cats.data.{Chain, OptionT}
import cats.effect.Async
import cats.implicits._
import cats.{MonadThrow, Monoid}
import co.topl.algebras._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models._
import co.topl.typeclasses.implicits._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized

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

  case class ConsensusData[F[_]](
    operatorStakes:   Store[F, StakingAddresses.Operator, Int128],
    totalActiveStake: Store[F, Unit, Int128],
    registrations:    Store[F, StakingAddresses.Operator, Box.Values.Registrations.Operator]
  )

  def make[F[_]: Async](
    currentBlockId:         F[TypedIdentifier],
    parentChildTree:        ParentChildTree[F, TypedIdentifier],
    currentEventChanged:    TypedIdentifier => F[Unit],
    initialState:           F[ConsensusData[F]],
    fetchBlockBody:         TypedIdentifier => F[BlockBody],
    fetchTransaction:       TypedIdentifier => F[Transaction],
    fetchTransactionOutput: Box.Id => F[Transaction.Output]
  ): F[EventSourcedState[F, ConsensusData[F], TypedIdentifier]] =
    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = new ApplyBlock(fetchBlockBody, fetchTransaction, fetchTransactionOutput),
      unapplyEvent = new UnapplyBlock(fetchBlockBody, fetchTransaction, fetchTransactionOutput),
      parentChildTree = parentChildTree,
      currentEventChanged
    )

  private class ApplyBlock[F[_]: MonadThrow](
    fetchBlockBody:         TypedIdentifier => F[BlockBody],
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
        newTotalStake = stakeChanges.map(_.delta).prepend(previousTotalStake.data).sumAll
        _ <- state.totalActiveStake.put((), Sized.maxUnsafe(newTotalStake))
        _ <- stakeChanges.traverseTap { case StakeChange(address, quantity) =>
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

    private def calculateStakeChanges(transaction: Transaction): F[Chain[StakeChange]] =
      for {
        inputAddressQuantities <- transaction.inputs
          .collect { case Transaction.Input(boxId, _, _, Box.Values.Arbit(quantity)) =>
            boxId -> -quantity.data
          }
          .traverse { case (boxId, quantity) =>
            fetchTransactionOutput(boxId).map(_.address.stakingAddress).tupleRight(quantity)
          }
        inputStakeChanges =
          inputAddressQuantities.collect { case (s: StakingAddresses.Operator, quantity) =>
            StakeChange(s, quantity)
          }
        outputStakeChanges = transaction.outputs.collect {
          case Transaction
                .Output(FullAddress(_, _, o: StakingAddresses.Operator, _), Box.Values.Arbit(quantity), _) =>
            StakeChange(o, quantity.data)
        }
        result = inputStakeChanges ++ outputStakeChanges
      } yield result

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
        operatorAddresses = allStakingAddresses.toIterable.collect { case o: StakingAddresses.Operator => o }.toSet
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
      } yield deregistrations ++ registrations

  }

  private class UnapplyBlock[F[_]: MonadThrow](
    fetchBlockBody:         TypedIdentifier => F[BlockBody],
    fetchTransaction:       TypedIdentifier => F[Transaction],
    fetchTransactionOutput: Box.Id => F[Transaction.Output]
  ) extends ((ConsensusData[F], TypedIdentifier) => F[ConsensusData[F]]) {

    def apply(state: ConsensusData[F], blockId: TypedIdentifier): F[ConsensusData[F]] =
      for {
        body                <- fetchBlockBody(blockId)
        transactions        <- body.toList.reverse.traverse(fetchTransaction)
        stakeChanges        <- transactions.foldMapM(calculateStakeChanges)
        registrationChanges <- transactions.foldMapM(calculateRegistrationChanges)
        previousTotalStake  <- state.totalActiveStake.getOrRaise(())
        newTotalStake = stakeChanges.map(_.delta).append(previousTotalStake.data).sumAll
        _ <- state.totalActiveStake.put((), Sized.maxUnsafe(newTotalStake))
        _ <- stakeChanges.traverseTap { stakeChange =>
          OptionT(state.operatorStakes.get(stakeChange.address))
            .fold(stakeChange.delta)(_.data + stakeChange.delta)
            .map[Int128](Sized.maxUnsafe)
            .flatMap(newQuantity => state.operatorStakes.put(stakeChange.address, newQuantity))
        }
        _ <- registrationChanges.toSeq.traverseTap {
          case (address, Some(registration)) =>
            state.registrations.put(address, registration)
          case (address, _) =>
            state.registrations.remove(address)
        }
      } yield state

    private def calculateStakeChanges(transaction: Transaction): F[Chain[StakeChange]] =
      for {
        outputStakeChanges <- transaction.outputs.reverse
          .collect {
            case Transaction
                  .Output(FullAddress(_, _, o: StakingAddresses.Operator, _), Box.Values.Arbit(quantity), _) =>
              StakeChange(o, -quantity.data)
          }
          .pure[F]
        inputAddressQuantities <- transaction.inputs.reverse
          .collect { case Transaction.Input(boxId, _, _, Box.Values.Arbit(quantity)) =>
            boxId -> quantity.data
          }
          .traverse { case (boxId, quantity) =>
            fetchTransactionOutput(boxId)
              .map(_.address.stakingAddress)
              .tupleRight(quantity)
          }
        inputStakeChanges =
          inputAddressQuantities.collect { case (s: StakingAddresses.Operator, quantity) =>
            StakeChange(s, quantity)
          }
      } yield outputStakeChanges ++ inputStakeChanges

    private def calculateRegistrationChanges(
      transaction: Transaction
    ): F[Map[StakingAddresses.Operator, Option[Box.Values.Registrations.Operator]]] =
      for {
        deregistrations <- transaction.outputs.reverse
          .collect {
            case Transaction.Output(
                  FullAddress(_, _, a: StakingAddresses.Operator, _),
                  _: Box.Values.Registrations.Operator,
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
        registrations = addressedBoxes.toIterable
          .flatMap(_.some.collect { case (o: StakingAddresses.Operator, value) => o -> value.some })
          .toMap
      } yield deregistrations ++ registrations
  }

  private case class StakeChange(address: StakingAddresses.Operator, delta: BigInt)

  implicit
  val registrationChangesMonoid: Monoid[Map[StakingAddresses.Operator, Option[Box.Values.Registrations.Operator]]] =
    Monoid.instance(Map.empty, _ ++ _)
}
