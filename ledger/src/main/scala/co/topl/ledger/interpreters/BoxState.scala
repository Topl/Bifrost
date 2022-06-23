package co.topl.ledger.interpreters

import cats.data.{NonEmptySet, OptionT}
import cats.effect.{Async, Sync}
import cats.implicits._
import cats.{Applicative, MonadThrow}
import co.topl.algebras.Store
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.BoxStateAlgebra
import co.topl.models.{BlockBodyV2, Box, Transaction, TypedIdentifier}
import co.topl.typeclasses.implicits._

import scala.collection.immutable.SortedSet

object BoxState {

  /**
   * Store Key: Transaction ID
   * Store Value: Array of Shorts, representing the currently spendable indices of the original transaction output array
   */
  type State[F[_]] = Store[F, TypedIdentifier, NonEmptySet[Short]]

  /**
   * Creates a BoxStateAlgebra interpreter that is backed by an event-sourced tree
   */
  def make[F[_]: Async](
    currentBlockId:   F[TypedIdentifier],
    fetchBlockBody:   TypedIdentifier => F[BlockBodyV2],
    fetchTransaction: TypedIdentifier => F[Transaction],
    parentChildTree:  ParentChildTree[F, TypedIdentifier],
    initialState:     F[State[F]]
  ): F[BoxStateAlgebra[F]] =
    for {
      eventSourcedState <- EventSourcedState.OfTree.make[F, State[F]](
        initialState,
        currentBlockId,
        applyEvent = applyBlock(fetchBlockBody, fetchTransaction),
        unapplyEvent = unapplyBlock(fetchBlockBody, fetchTransaction),
        parentChildTree
      )
    } yield new BoxStateAlgebra[F] {

      def boxExistsAt(blockId: TypedIdentifier)(boxId: Box.Id): F[Boolean] =
        eventSourcedState
          .useStateAt(blockId)(_.get(boxId.transactionId))
          .map(_.exists(_.contains(boxId.transactionOutputIndex)))
    }

  /**
   * Apply the given block to the state.
   *
   * - For each transaction
   *   - Each input is removed from the state
   *   - Each output is added to the state
   */
  private def applyBlock[F[_]: MonadThrow](
    fetchBlockBody:   TypedIdentifier => F[BlockBodyV2],
    fetchTransaction: TypedIdentifier => F[Transaction]
  )(state:            State[F], blockId: TypedIdentifier): F[State[F]] =
    for {
      body         <- fetchBlockBody(blockId).map(_.toList)
      transactions <- body.traverse(fetchTransaction)
      _ <- transactions.traverse(transaction =>
        transaction.inputs.traverse(input =>
          state
            .getOrRaise(input.boxId.transactionId)
            .flatMap(unspentIndices =>
              OptionT
                .fromOption[F](NonEmptySet.fromSet(unspentIndices - input.boxId.transactionOutputIndex))
                .foldF(state.remove(input.boxId.transactionId))(state.put(input.boxId.transactionId, _))
            )
        ) >> OptionT
          .fromOption[F](
            NonEmptySet.fromSet(SortedSet.from(transaction.outputs.void.zipWithIndex.map(_._2.toShort).toIterable))
          )
          .foldF(Applicative[F].unit)(state.put(transaction.id, _))
      )
    } yield state

  /**
   * Unapply the given block from the state.
   *
   * - For each transaction
   *   - Each output is removed from the state
   *   - Each input is added to the state
   */
  private def unapplyBlock[F[_]: MonadThrow](
    fetchBlockBody:   TypedIdentifier => F[BlockBodyV2],
    fetchTransaction: TypedIdentifier => F[Transaction]
  )(state:            State[F], blockId: TypedIdentifier): F[State[F]] =
    for {
      body         <- fetchBlockBody(blockId).map(_.toList)
      transactions <- body.traverse(fetchTransaction)
      _ <- transactions.traverse(transaction =>
        state.remove(transaction.id) >>
        transaction.inputs.traverse(input =>
          state
            .getOrRaise(input.boxId.transactionId)
            .map(_.add(input.boxId.transactionOutputIndex))
            .flatMap(state.put(input.boxId.transactionId, _))
        )
      )
    } yield state
}

object AugmentedBoxState {

  /**
   * Constructs a BoxState that wraps another underlying BoxState.  The new BoxState intercepts calls to boxExistsAt
   * and checks to see if the values are included in the given StateAugmentation object.  StateAugmentation acts as
   * a simple in-memory tracker of BoxIds that have been spent and created.  It is intended to allow box existence
   * checks while considering the history of transactions _within_ the block that is being validated
   * @param boxState an underlying BoxState
   * @param stateAugmentation a set of BoxState changes to consider when intercepting box existence checks
   */
  def make[F[_]: Sync](boxState: BoxStateAlgebra[F])(stateAugmentation: StateAugmentation): F[BoxStateAlgebra[F]] =
    Sync[F].delay {
      new BoxStateAlgebra[F] {
        def boxExistsAt(blockId: TypedIdentifier)(boxId: Box.Id): F[Boolean] =
          if (stateAugmentation.newBoxIds.contains(boxId)) true.pure[F]
          else if (stateAugmentation.spentBoxIds.contains(boxId)) false.pure[F]
          else boxState.boxExistsAt(blockId)(boxId)
      }
    }

  /**
   * Establishes an ephemeral set of spent box IDs and new box IDs.  This augmentation can be further augmented
   * by including an additional transaction.
   * @param spentBoxIds a set of box IDs that are no longer eligibile to be spent
   * @param newBoxIds a set of box IDs that have been created _and_ have not been spent
   */
  case class StateAugmentation(spentBoxIds: Set[Box.Id], newBoxIds: Set[Box.Id]) {

    /**
     * Returns a new StateAugmentation which includes the inputs and outputs of the given transaction.  If
     * the Transaction spends a box that exists in `newBoxIds`, the entry is moved from `newBoxIds` to `spentBoxIds`.
     */
    def augment(transaction: Transaction): StateAugmentation = {
      val transactionSpentBoxIds = transaction.inputs.map(_.boxId).toIterable.toSet
      val transactionId = transaction.id.asTypedBytes
      val transactionNewBoxIds =
        transaction.outputs.mapWithIndex((_, idx) => Box.Id(transactionId, idx.toShort)).toIterable.toSet
      StateAugmentation(
        spentBoxIds ++ transactionSpentBoxIds,
        newBoxIds -- transactionSpentBoxIds -- transactionNewBoxIds
      )
    }

  }

  object StateAugmentation {
    val empty: StateAugmentation = StateAugmentation(Set.empty, Set.empty)
  }
}
