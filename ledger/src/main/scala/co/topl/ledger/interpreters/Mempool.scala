package co.topl.ledger.interpreters

import cats.effect.{Async, Ref}
import cats.implicits._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.{BlockBodyV2, TypedIdentifier}

object Mempool {

  def make[F[_]: Async](
    currentBlockId:  F[TypedIdentifier],
    fetchBlockBody:  TypedIdentifier => F[BlockBodyV2],
    parentChildTree: ParentChildTree[F, TypedIdentifier]
  ): F[MempoolAlgebra[F]] =
    for {
      unconfirmedTransactionsRef <- Ref.of(Set.empty[TypedIdentifier])
      eventSourcedState <- EventSourcedState.OfTree.make[F, Ref[F, Set[TypedIdentifier]]](
        unconfirmedTransactionsRef.pure[F],
        currentBlockId,
        (ref, blockId) =>
          // TODO: Check all other transactions in the mempool to see if they attempt to spend the same inputs
          // as the given block.  If so, schedule (a very eager) eviction
          fetchBlockBody(blockId).flatMap(blockBody =>
            ref.update(unconfirmedTransactionIds => unconfirmedTransactionIds -- blockBody).as(ref)
          ),
        (ref, blockId) =>
          fetchBlockBody(blockId).flatMap(blockBody =>
            ref.update(unconfirmedTransactionIds => unconfirmedTransactionIds ++ blockBody).as(ref)
          ),
        parentChildTree
      )
    } yield new MempoolAlgebra[F] {

      def read(blockId: TypedIdentifier): F[Set[TypedIdentifier]] =
        eventSourcedState.stateAt(blockId).flatMap(_.get)

      def add(transactionId: TypedIdentifier): F[Unit] =
        unconfirmedTransactionsRef.update(_ + transactionId)

      def remove(transactionId: TypedIdentifier): F[Unit] =
        unconfirmedTransactionsRef.update(_ - transactionId)
    }
}
