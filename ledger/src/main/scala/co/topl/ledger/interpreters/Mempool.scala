package co.topl.ledger.interpreters

import cats.effect.kernel.Spawn
import cats.effect.{Async, Ref}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.{BlockBodyV2, Transaction, TypedIdentifier}

// TODO: Non-minting nodes?
object Mempool {

  def make[F[_]: Async: Spawn](
    currentBlockId:  F[TypedIdentifier],
    fetchBlockBody:  TypedIdentifier => F[BlockBodyV2],
    parentChildTree: ParentChildTree[F, TypedIdentifier],
    getTransaction:  TypedIdentifier => F[Transaction],
    clock:           ClockAlgebra[F]
  ): F[MempoolAlgebra[F]] =
    for {
      unconfirmedTransactionsRef <- Ref.of(Map.empty[TypedIdentifier, () => Unit])
      addTransaction = (transactionId: TypedIdentifier) =>
        for {
          transaction       <- getTransaction(transactionId)
          (delayed, cancel) <- clock.delayedUntilSlot(transaction.chronology.maximumSlot)
          _ <- Spawn[F].start(delayed.flatMap(_ => unconfirmedTransactionsRef.update(_ - transactionId)))
          _ <- unconfirmedTransactionsRef.update(_.updated(transactionId, cancel))
        } yield ()
      eventSourcedState <- EventSourcedState.OfTree.make[F, Ref[F, Map[TypedIdentifier, () => Unit]]](
        unconfirmedTransactionsRef.pure[F],
        currentBlockId,
        (ref, blockId) =>
          // TODO: Check all other transactions in the mempool to see if they attempt to spend the same inputs
          // as the given block.  If so, schedule (a very eager) eviction
          for {
            blockBody           <- fetchBlockBody(blockId)
            expirationsToCancel <- ref.get.map(entries => blockBody.map(entries))
            _                   <- Async[F].delay(expirationsToCancel.foreach(f => f()))
            _                   <- ref.update(unconfirmedTransactionIds => unconfirmedTransactionIds -- blockBody)
          } yield ref,
        (ref, blockId) => fetchBlockBody(blockId).flatMap(_.traverse(addTransaction)).as(ref),
        parentChildTree
      )
    } yield new MempoolAlgebra[F] {

      def read(blockId: TypedIdentifier): F[Set[TypedIdentifier]] =
        eventSourcedState.stateAt(blockId).flatMap(_.get).map(_.keySet)

      def add(transactionId: TypedIdentifier): F[Unit] =
        addTransaction(transactionId)

      def remove(transactionId: TypedIdentifier): F[Unit] =
        unconfirmedTransactionsRef.update(_ - transactionId)
    }
}
