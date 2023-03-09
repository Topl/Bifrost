package co.topl.consensus.interpreters

import cats.data.Chain
import cats.data.OptionT
import cats.effect.Async
import cats.implicits._
import cats.MonadThrow
import cats.Monoid
import co.topl.algebras._
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.consensus.models.SignatureKesProduct
import co.topl.eventtree.EventSourcedState
import co.topl.eventtree.ParentChildTree
import co.topl.models.StakingAddress
import co.topl.node.models.BlockBody
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._

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
    operatorStakes:   Store[F, StakingAddress, BigInt],
    totalActiveStake: Store[F, Unit, BigInt],
    registrations:    Store[F, StakingAddress, SignatureKesProduct]
  )

  def make[F[_]: Async](
    currentBlockId:      F[BlockId],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[ConsensusData[F]],
    fetchBlockBody:      BlockId => F[BlockBody],
    fetchTransaction:    Identifier.IoTransaction32 => F[IoTransaction]
  ): F[EventSourcedState[F, ConsensusData[F], BlockId]] =
    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = new ApplyBlock(fetchBlockBody, fetchTransaction),
      unapplyEvent = new UnapplyBlock(fetchBlockBody, fetchTransaction),
      parentChildTree = parentChildTree,
      currentEventChanged
    )

  private class ApplyBlock[F[_]: MonadThrow](
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: Identifier.IoTransaction32 => F[IoTransaction]
  ) extends ((ConsensusData[F], BlockId) => F[ConsensusData[F]]) {

    def apply(state: ConsensusData[F], blockId: BlockId): F[ConsensusData[F]] =
      for {
        body                <- fetchBlockBody(blockId)
        transactions        <- body.transactionIds.traverse(fetchTransaction)
        stakeChanges        <- transactions.foldMapM(calculateStakeChanges)
        registrationChanges <- transactions.foldMapM(calculateRegistrationChanges)
        previousTotalStake  <- state.totalActiveStake.getOrRaise(())
        newTotalStake = stakeChanges.map(_.delta).prepend(previousTotalStake).sumAll
        _ <- state.totalActiveStake.put((), newTotalStake)
        _ <- stakeChanges.traverseTap { case StakeChange(address, quantity) =>
          OptionT(state.operatorStakes.get(address))
            .fold(quantity)(_ + quantity)
            .flatMap(newQuantity => state.operatorStakes.put(address, newQuantity))
        }
        _ <- registrationChanges.toSeq.traverseTap {
          case (address, Some(registration)) =>
            state.registrations.put(address, registration)
          case (address, _) =>
            state.registrations.remove(address)
        }
      } yield state

    private def calculateStakeChanges(transaction: IoTransaction): F[Chain[StakeChange]] =
      for {
        inputStakeChanges <- transaction.inputs
          .flatMap(_.value.value.topl)
          .filterNot(_.stakingAddress.isEmpty)
          .map(v => v.stakingAddress -> -(v.quantity: BigInt))
          .toMap
          .pure[F]
        outputStakeChanges = transaction.outputs
          .flatMap(_.value.value.topl)
          .filterNot(_.stakingAddress.isEmpty)
          .map(v => v.stakingAddress -> (v.quantity: BigInt))
          .toMap
        result = Chain
          .fromIterableOnce(inputStakeChanges ++ outputStakeChanges)
          .map { case (address, delta) =>
            StakeChange(address, delta)
          }
      } yield result

    private def calculateRegistrationChanges(
      transaction: IoTransaction
    ): F[Map[StakingAddress, Option[SignatureKesProduct]]] =
      for {
        deregistrations <- transaction.inputs
          .flatMap(_.value.value.registration)
          .map(_.stakingAddress)
          .tupleRight(none[SignatureKesProduct])
          .toMap
          .pure[F]
        registrations = transaction.outputs
          .flatMap(_.value.value.registration)
          .map(r => r.stakingAddress -> r.registration.some)
          .toMap
      } yield deregistrations ++ registrations

  }

  private class UnapplyBlock[F[_]: MonadThrow](
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: Identifier.IoTransaction32 => F[IoTransaction]
  ) extends ((ConsensusData[F], BlockId) => F[ConsensusData[F]]) {

    def apply(state: ConsensusData[F], blockId: BlockId): F[ConsensusData[F]] =
      for {
        body                <- fetchBlockBody(blockId)
        transactions        <- body.transactionIds.reverse.traverse(fetchTransaction)
        stakeChanges        <- transactions.foldMapM(calculateStakeChanges)
        registrationChanges <- transactions.foldMapM(calculateRegistrationChanges)
        previousTotalStake  <- state.totalActiveStake.getOrRaise(())
        newTotalStake = stakeChanges.map(_.delta).append(previousTotalStake).sumAll
        _ <- state.totalActiveStake.put((), newTotalStake)
        _ <- stakeChanges.traverseTap { stakeChange =>
          OptionT(state.operatorStakes.get(stakeChange.address))
            .fold(stakeChange.delta)(_ + stakeChange.delta)
            .flatMap(newQuantity => state.operatorStakes.put(stakeChange.address, newQuantity))
        }
        _ <- registrationChanges.toSeq.traverseTap {
          case (address, Some(registration)) =>
            state.registrations.put(address, registration)
          case (address, _) =>
            state.registrations.remove(address)
        }
      } yield state

    private def calculateStakeChanges(transaction: IoTransaction): F[Chain[StakeChange]] =
      for {
        outputStakeChanges <- transaction.outputs.reverse
          .flatMap(_.value.value.topl.filterNot(_.stakingAddress.isEmpty))
          .map(v => v.stakingAddress -> -(v.quantity: BigInt))
          .pure[F]
        inputStakeChanges = transaction.inputs.reverse
          .flatMap(_.value.value.topl.filterNot(_.stakingAddress.isEmpty))
          .map(v => v.stakingAddress -> (v.quantity: BigInt))
        result = Chain
          .fromIterableOnce(inputStakeChanges ++ outputStakeChanges)
          .map { case (address, delta) =>
            StakeChange(address, delta)
          }
      } yield result

    private def calculateRegistrationChanges(
      transaction: IoTransaction
    ): F[Map[StakingAddress, Option[SignatureKesProduct]]] =
      for {
        deregistrations <- transaction.outputs.reverse
          .flatMap(_.value.value.registration)
          .map(_.stakingAddress -> none[SignatureKesProduct])
          .toMap
          .pure[F]
        registrations = transaction.inputs.reverse
          .flatMap(_.value.value.registration)
          .map(r => r.stakingAddress -> r.registration.some)
          .toMap
      } yield deregistrations ++ registrations
  }

  private case class StakeChange(address: StakingAddress, delta: BigInt)

  implicit val registrationChangesMonoid: Monoid[Map[StakingAddress, Option[SignatureKesProduct]]] =
    Monoid.instance(Map.empty, _ ++ _)
}
