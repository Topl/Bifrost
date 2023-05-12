package co.topl.ledger.interpreters

import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.consensus.models.BlockId
import co.topl.eventtree.EventSourcedState
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._

object Mempool {

  type State[F[_]] = Ref[F, MempoolGraph]

  def make[F[_]: Async](
    currentBlockId:         F[BlockId],
    fetchBody:              BlockId => F[BlockBody],
    fetchTransaction:       TransactionId => F[IoTransaction],
    parentChildTree:        ParentChildTree[F, BlockId],
    currentEventChanged:    BlockId => F[Unit],
    clock:                  ClockAlgebra[F],
    onExpiration:           TransactionId => F[Unit],
    defaultExpirationLimit: Long
  ): Resource[F, MempoolAlgebra[F]] =
    for {
      graphState <- Ref.of(MempoolGraph(Map.empty, Map.empty, Map.empty)).toResource
      expirationsState <- Resource.make(Ref.of(Map.empty[TransactionId, Fiber[F, Throwable, Unit]]))(
        _.get.flatMap(_.values.toList.traverse(_.cancel).void)
      )
      expireTransaction = (transaction: IoTransaction) =>
        graphState
          .modify(_.removeSubtree(transaction))
          .map(_.map(_.id).toList)
          .flatMap(expired =>
            expirationsState.update(_.removedAll(expired)) *>
            expired.traverse(onExpiration)
          )
      addWithExpiration = (transaction: IoTransaction) =>
        for {
          currentSlot <- clock.globalSlot
          expirationSlot = transaction.datum.event.schedule.max.min(currentSlot + defaultExpirationLimit)
          expirationFiber <-
            clock
              .delayedUntilSlot(expirationSlot)
              .flatMap(_ => expireTransaction(transaction))
              .void
              .start
          _ <- expirationsState.update(_.updated(transaction.id, expirationFiber))
          _ <- graphState.update(_.add(transaction))
        } yield ()
      removeWithExpiration = (transaction: IoTransaction) =>
        graphState
          .modify(_.removeSubtree(transaction))
          .map(_.map(_.id))
          .flatTap(removed =>
            expirationsState
              .getAndUpdate(_.removedAll(removed))
              .flatTap(expirations => removed.flatMap(expirations.get).toList.traverse(_.cancel))
          )
          .void
      applyBlock = (state: State[F], blockId: BlockId) =>
        for {
          body <- fetchBody(blockId)
          _    <- body.transactionIds.traverse(fetchTransaction(_).flatMap(removeWithExpiration))
        } yield state
      unapplyBlock = (state: State[F], blockId: BlockId) =>
        for {
          body <- fetchBody(blockId)
          _    <- body.transactionIds.traverse(fetchTransaction(_).flatMap(addWithExpiration))
        } yield state
      eventSourcedState <- EventSourcedState.OfTree
        .make[F, State[F], BlockId](
          graphState.pure[F],
          currentBlockId,
          applyEvent = applyBlock,
          unapplyEvent = unapplyBlock,
          parentChildTree,
          currentEventChanged
        )
        .toResource
    } yield new MempoolAlgebra[F] {

      def read(blockId: BlockId): F[Set[TransactionId]] =
        eventSourcedState
          .useStateAt(blockId)(_.get)
          .map(graph =>
            // TODO: Traversal
            graph.transactions.keySet
          )

      def add(transactionId: TransactionId): F[Unit] =
        fetchTransaction(transactionId).flatMap(addWithExpiration)

      def remove(transactionId: TransactionId): F[Unit] =
        fetchTransaction(transactionId).flatMap(removeWithExpiration)

      def contains(blockId: BlockId, transactionId: TransactionId): F[Boolean] =
        eventSourcedState
          .useStateAt(blockId)(_.get)
          .map(_.transactions.contains(transactionId))
    }

}
