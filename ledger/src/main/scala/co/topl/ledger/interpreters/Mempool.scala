package co.topl.ledger.interpreters

import cats.effect.{Async, Ref}
import cats.implicits._
import co.topl.algebras.Store
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.{BlockBodyV2, TypedIdentifier}

object Mempool {

  def make[F[_]: Async](
    currentBlockId:        F[TypedIdentifier],
    blockBodyStore:        Store[F, TypedIdentifier, BlockBodyV2],
    unapplyBlockBodyStore: Store[F, TypedIdentifier, BlockBodyV2],
    parentChildTree:       ParentChildTree[F, TypedIdentifier]
  ): F[MempoolAlgebra[F]] =
    for {
      unconfirmedTransactionsRef <- Ref.of(Set.empty[TypedIdentifier])
      eventSourcedState <- EventSourcedState.OfTree.make[F, BlockBodyV2, Ref[F, Set[TypedIdentifier]], BlockBodyV2](
        unconfirmedTransactionsRef.pure[F],
        currentBlockId,
        (blockBody, _) => blockBody.pure[F],
        (ref, blockBody) => ref.update(unconfirmedTransactionIds => unconfirmedTransactionIds -- blockBody).as(ref),
        (ref, blockBody) => ref.update(unconfirmedTransactionIds => unconfirmedTransactionIds ++ blockBody).as(ref),
        blockBodyStore,
        unapplyBlockBodyStore,
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
