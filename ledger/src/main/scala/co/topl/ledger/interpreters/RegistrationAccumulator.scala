package co.topl.ledger.interpreters

import cats.MonadThrow
import cats.effect.{Async, Resource, Sync}
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.{BlockId, StakingAddress}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.RegistrationAccumulatorAlgebra
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._

/**
 * A RegistrationAccumulatorAlgebra interpreter which uses an EventSourcedState to track a set of Staking Addresses.
 * Each transaction in each block contains several inputs which spend (remove) addresses from the set, and several outputs
 * which create (add) addresses to the set.
 */
object RegistrationAccumulator {

  type State[F[_]] = Store[F, StakingAddress, Unit]

  def make[F[_]: Async](
    currentBlockId:      F[BlockId],
    fetchBlockBody:      BlockId => F[BlockBody],
    fetchTransaction:    TransactionId => F[IoTransaction],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[State[F]]
  ): Resource[F, RegistrationAccumulatorAlgebra[F]] =
    for {
      eventSourcedState <- EventSourcedState.OfTree
        .make[F, State[F], BlockId](
          initialState,
          currentBlockId,
          applyEvent = applyBlock(fetchBlockBody, fetchTransaction),
          unapplyEvent = unapplyBlock(fetchBlockBody, fetchTransaction),
          parentChildTree,
          currentEventChanged
        )
        .toResource
    } yield new RegistrationAccumulatorAlgebra[F] {

      def contains(blockId: BlockId)(address: StakingAddress): F[Boolean] =
        eventSourcedState.useStateAt(blockId)(_.contains(address))
    }

  /**
   * Apply the given block to the state.
   *
   * - For each transaction
   *   - Remove each "spent" registration from the state
   *   - Add each "created" registration to the state
   */
  private def applyBlock[F[_]: MonadThrow](
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  )(state: State[F], blockId: BlockId): F[State[F]] =
    for {
      body <- fetchBlockBody(blockId)
      _ <- body.transactionIds
        .traverse(
          fetchTransaction(_)
            .flatMap(transaction =>
              // Construct a Map by stepping through inputs and marking addresses for removal, then stepping through
              // the outputs marking addresses for addition (while overriding the inputs)
              (
                transaction.inputs.flatMap(_.value.value.topl.flatMap(_.registration)).tupleRight(false) ++
                transaction.outputs.flatMap(_.value.value.topl.flatMap(_.registration)).tupleRight(true)
              ).toMap.toList
                .traverse {
                  case (registration, true)  => state.put(registration.address, ())
                  case (registration, false) => state.remove(registration.address)
                }
            )
        )
    } yield state

  /**
   * Unapply the given block from the state.
   *
   * - Reverse transactions, and for each transaction
   *   - Remove each "created" registration from the state
   *   - Add each "spent" registration to the state
   */
  private def unapplyBlock[F[_]: MonadThrow](
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  )(state: State[F], blockId: BlockId): F[State[F]] =
    for {
      body <- fetchBlockBody(blockId)
      _ <- body.transactionIds.reverse
        .traverse(
          fetchTransaction(_)
            .flatMap(transaction =>
              // Similar to "applyBlock", but reversed.  Start with the reversed outputs (marked for deletion), then step
              // through the reversed inputs (marked for addition) and override the outputs.
              (
                transaction.outputs.reverse.flatMap(_.value.value.topl.flatMap(_.registration)).tupleRight(false) ++
                transaction.inputs.reverse.flatMap(_.value.value.topl.flatMap(_.registration)).tupleRight(true)
              ).toMap.toList
                .traverse {
                  case (registration, true)  => state.put(registration.address, ())
                  case (registration, false) => state.remove(registration.address)
                }
            )
        )
    } yield state

  /**
   * Implements a RegistrationAccumulatorAlgebra which wraps another RegistrationAccumulatorAlgebra.  This implementation
   * allows for dynamically constructing a new interpreter while folding each transaction in a block.
   */
  object Augmented {

    def make[F[_]: Sync](
      state: RegistrationAccumulatorAlgebra[F]
    )(augmentation: Augmentation): Resource[F, RegistrationAccumulatorAlgebra[F]] =
      Resource.pure {
        new RegistrationAccumulatorAlgebra[F] {
          def contains(blockId: BlockId)(address: StakingAddress): F[Boolean] =
            if (augmentation.newRegistrationAddresses.contains(address)) true.pure[F]
            else if (augmentation.spentRegistrationAddresses.contains(address)) false.pure[F]
            else state.contains(blockId)(address)
        }
      }
  }

  case class Augmentation(
    spentRegistrationAddresses: Set[StakingAddress],
    newRegistrationAddresses:   Set[StakingAddress]
  ) {

    /**
     * Returns a new StateAugmentation which considers registrations on the inputs and registrations on the outputs.  Spent
     * registrations are removed from the state, and new registrations are added to the state.
     */
    def augment(transaction: IoTransaction): Augmentation = {
      val txSpentAddresses = transaction.inputs.flatMap(_.value.value.topl.flatMap(_.registration).map(_.address)).toSet
      val txNewAddresses = transaction.outputs.flatMap(_.value.value.topl.flatMap(_.registration).map(_.address)).toSet
      Augmentation(
        spentRegistrationAddresses ++ txSpentAddresses,
        (newRegistrationAddresses ++ txNewAddresses) -- txSpentAddresses
      )
    }
  }

  object Augmentation {
    val empty: Augmentation = Augmentation(Set.empty, Set.empty)
  }

}
