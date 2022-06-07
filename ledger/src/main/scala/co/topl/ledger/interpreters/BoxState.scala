package co.topl.ledger.interpreters

import cats.data.{NonEmptySet, OptionT}
import cats.effect.Async
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

  type State[F[_]] = Store[F, TypedIdentifier, NonEmptySet[Short]]

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
      body         <- fetchBlockBody(blockId)
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
      body         <- fetchBlockBody(blockId)
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
