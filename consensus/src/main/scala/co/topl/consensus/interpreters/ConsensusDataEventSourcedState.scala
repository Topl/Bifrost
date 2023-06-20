package co.topl.consensus.interpreters

import cats.MonadThrow
import cats.effect.Async
import cats.implicits._
import co.topl.algebras._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.{ActiveStaker, BlockId, StakingAddress}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.node.models.BlockBody
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
    totalActiveStake:   Store[F, Unit, BigInt],
    totalInactiveStake: Store[F, Unit, BigInt],
    stakers:            Store[F, StakingAddress, ActiveStaker]
  )

  def make[F[_]: Async](
    currentBlockId:      F[BlockId],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[ConsensusData[F]],
    fetchBlockBody:      BlockId => F[BlockBody],
    fetchTransaction:    TransactionId => F[IoTransaction]
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
    fetchTransaction: TransactionId => F[IoTransaction]
  ) extends ((ConsensusData[F], BlockId) => F[ConsensusData[F]]) {

    def apply(state: ConsensusData[F], blockId: BlockId): F[ConsensusData[F]] =
      for {
        body         <- fetchBlockBody(blockId)
        transactions <- body.transactionIds.traverse(fetchTransaction)
        spentActiveStake = activeQuantityOf(transactions.flatMap(_.inputs).map(_.value))
        createdActiveStake = activeQuantityOf(transactions.flatMap(_.outputs).map(_.value))
        previousTotalActiveStake <- state.totalActiveStake.getOrRaise(())
        _ <- state.totalActiveStake.put((), previousTotalActiveStake - spentActiveStake + createdActiveStake)
        spentInactiveStake = inactiveQuantityOf(transactions.flatMap(_.inputs).map(_.value))
        createdInactiveStake = inactiveQuantityOf(transactions.flatMap(_.outputs).map(_.value))
        previousTotalInactiveStake <- state.totalInactiveStake.getOrRaise(())
        _ <- state.totalInactiveStake.put((), previousTotalInactiveStake - spentInactiveStake + createdInactiveStake)
        removedRegistrations = transactions.flatMap(removedStakersOf)
        addedRegistrations = transactions.flatMap(addedStakersOf)
        _ <- removedRegistrations.map(_.registration.address).traverseTap(state.stakers.remove)
        _ <- addedRegistrations.traverseTap(r => state.stakers.put(r.registration.address, r))
      } yield state

    private def removedStakersOf(transaction: IoTransaction): List[ActiveStaker] =
      transaction.inputs
        .flatMap(_.value.value.topl)
        .flatMap(t => t.registration.map(ActiveStaker(_, t.quantity)))
        .toList

    private def addedStakersOf(transaction: IoTransaction): List[ActiveStaker] =
      transaction.outputs
        .flatMap(_.value.value.topl)
        .flatMap(t => t.registration.map(ActiveStaker(_, t.quantity)))
        .toList

  }

  private class UnapplyBlock[F[_]: MonadThrow](
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  ) extends ((ConsensusData[F], BlockId) => F[ConsensusData[F]]) {

    def apply(state: ConsensusData[F], blockId: BlockId): F[ConsensusData[F]] =
      for {
        body         <- fetchBlockBody(blockId)
        transactions <- body.transactionIds.reverse.traverse(fetchTransaction)
        spentActiveStake = activeQuantityOf(transactions.flatMap(_.inputs.reverse).map(_.value))
        createdActiveStake = activeQuantityOf(transactions.flatMap(_.outputs.reverse).map(_.value))
        previousTotalActiveStake <- state.totalActiveStake.getOrRaise(())
        _ <- state.totalActiveStake.put((), previousTotalActiveStake + spentActiveStake - createdActiveStake)
        spentInactiveStake = inactiveQuantityOf(transactions.flatMap(_.inputs.reverse).map(_.value))
        createdInactiveStake = inactiveQuantityOf(transactions.flatMap(_.outputs.reverse).map(_.value))
        previousTotalInactiveStake <- state.totalInactiveStake.getOrRaise(())
        _ <- state.totalInactiveStake.put((), previousTotalInactiveStake + spentInactiveStake - createdInactiveStake)
        addedStakers = transactions.flatMap(addedStakersOf)
        removedStakers = transactions.flatMap(removedStakersOf)
        _ <- addedStakers.map(_.registration.address).traverseTap(state.stakers.remove)
        _ <- removedStakers.traverseTap(r => state.stakers.put(r.registration.address, r))
      } yield state

    private def removedStakersOf(transaction: IoTransaction): List[ActiveStaker] =
      transaction.inputs.reverse
        .flatMap(_.value.value.topl)
        .flatMap(t => t.registration.map(ActiveStaker(_, t.quantity)))
        .toList

    private def addedStakersOf(transaction: IoTransaction): List[ActiveStaker] =
      transaction.outputs.reverse
        .flatMap(_.value.value.topl)
        .flatMap(t => t.registration.map(ActiveStaker(_, t.quantity)))
        .toList

  }

  private def activeQuantityOf(values: Seq[Value]) =
    values
      .flatMap(_.value.topl)
      .filter(_.registration.nonEmpty)
      .map(_.quantity.value.toByteArray)
      .foldMap(BigInt(_))

  private def inactiveQuantityOf(values: Seq[Value]) =
    values
      .flatMap(_.value.topl)
      .filter(_.registration.isEmpty)
      .map(_.quantity.value.toByteArray)
      .foldMap(BigInt(_))
}
