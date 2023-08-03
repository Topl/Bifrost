package co.topl.ledger.interpreters

import cats.data.{NonEmptySet, OptionT}
import cats.effect.Async
import cats.implicits._
import cats.{Applicative, MonadThrow}
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.consensus.models.BlockId
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.BoxStateAlgebra
import co.topl.models.utility._
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._

import scala.collection.immutable.SortedSet

object BoxState {

  /**
   * Store Key: Transaction ID
   * Store Value: Array of Shorts, representing the currently spendable indices of the original transaction output array
   */
  type State[F[_]] = Store[F, TransactionId, NonEmptySet[Short]]

  /**
   * Creates a BoxStateAlgebra interpreter that is backed by an event-sourced tree
   */
  def make[F[_]: Async](
    currentBlockId:      F[BlockId],
    fetchBlockBody:      BlockId => F[BlockBody],
    fetchTransaction:    TransactionId => F[IoTransaction],
    parentChildTree:     ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit],
    initialState:        F[State[F]]
  ): F[BoxStateAlgebra[F]] =
    for {
      eventSourcedState <- EventSourcedState.OfTree.make[F, State[F], BlockId](
        initialState,
        currentBlockId,
        applyEvent = applyBlock(fetchBlockBody, fetchTransaction),
        unapplyEvent = unapplyBlock(fetchBlockBody, fetchTransaction),
        parentChildTree,
        currentEventChanged
      )
    } yield new BoxStateAlgebra[F] {

      def boxExistsAt(blockId: BlockId)(boxId: TransactionOutputAddress): F[Boolean] =
        eventSourcedState
          .useStateAt(blockId)(_.get(boxId.id))
          .map(_.exists(_.contains(boxId.index.toShort)))
    }

  /**
   * Apply the given block to the state.
   *
   * - For each transaction
   *   - Each input is removed from the state
   *   - Each output is added to the state
   */
  private def applyBlock[F[_]: MonadThrow](
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  )(state: State[F], blockId: BlockId): F[State[F]] =
    for {
      body <- fetchBlockBody(blockId)
      _ <- body.transactionIds.traverse(transactionId =>
        fetchTransaction(transactionId).flatMap(transaction =>
          transaction.inputs.traverse { input =>
            val txId = input.address.id
            state
              .getOrRaise(txId)
              .flatMap(unspentIndices =>
                OptionT
                  .fromOption[F](
                    NonEmptySet.fromSet(unspentIndices - input.address.index.toShort)
                  )
                  .foldF(state.remove(txId))(state.put(txId, _))
              )
          } >> OptionT
            .fromOption[F](
              NonEmptySet.fromSet(SortedSet.from(transaction.outputs.indices.map(_.toShort)))
            )
            .foldF(Applicative[F].unit)(state.put(transaction.id, _))
        )
      )
      // The reward transaction should have exactly one spendable output
      _ <- body.rewardTransactionId.traverse(state.put(_, NonEmptySet.one(0: Short)))
    } yield state

  /**
   * Unapply the given block from the state.
   *
   * - For each transaction
   *   - Each output is removed from the state
   *   - Each input is added to the state
   */
  private def unapplyBlock[F[_]: MonadThrow](
    fetchBlockBody:   BlockId => F[BlockBody],
    fetchTransaction: TransactionId => F[IoTransaction]
  )(state: State[F], blockId: BlockId): F[State[F]] =
    for {
      body <- fetchBlockBody(blockId)
      _ <- body.transactionIds.traverse(transaction =>
        fetchTransaction(transaction).flatMap(transaction =>
          state.remove(transaction.id) >>
          transaction.inputs.traverse { input =>
            val txId = input.address.id
            OptionT(state.get(txId))
              .fold(NonEmptySet.one(input.address.index.toShort))(
                _.add(input.address.index.toShort)
              )
              .flatMap(state.put(txId, _))
          }
        )
      )
      _ <- body.rewardTransactionId.traverse(state.remove)
    } yield state
}
