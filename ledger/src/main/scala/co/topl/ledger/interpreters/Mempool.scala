package co.topl.ledger.interpreters

import cats.data.EitherT
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.validation.algebras.TransactionCostCalculator
import co.topl.consensus.models.BlockId
import co.topl.eventtree.EventSourcedState
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.algebras.{MempoolAlgebra, TransactionRewardCalculatorAlgebra}
import co.topl.ledger.models.MempoolGraph
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import fs2.concurrent.Topic

object Mempool {

  type State[F[_]] = Ref[F, MempoolGraph]

  def make[F[_]: Async](
    currentBlockId:              F[BlockId],
    fetchBody:                   BlockId => F[BlockBody],
    fetchTransaction:            TransactionId => F[IoTransaction],
    parentChildTree:             ParentChildTree[F, BlockId],
    currentEventChanged:         BlockId => F[Unit],
    clock:                       ClockAlgebra[F],
    onExpiration:                TransactionId => F[Unit],
    defaultExpirationLimit:      Long,
    transactionRewardCalculator: TransactionRewardCalculatorAlgebra,
    txCostCalculator:            TransactionCostCalculator
  ): Resource[F, (MempoolAlgebra[F], EventSourcedState[F, State[F], BlockId])] =
    for {
      graphState <- Ref
        .of(MempoolGraph(Map.empty, Map.empty, Map.empty, transactionRewardCalculator, txCostCalculator))
        .toResource
      expirationsState <- Resource.make(Ref.of(Map.empty[TransactionId, Fiber[F, Throwable, Unit]]))(
        _.get.flatMap(_.values.toList.traverse(_.cancel).void)
      )
      adoptionsTopic <- Resource.make(Topic[F, TransactionId])(_.close.void)
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
          .update(_.removeSingle(transaction)) *> expirationsState
          .getAndUpdate(_.removed(transaction.id))
          .flatTap(_.get(transaction.id).traverse(_.cancel))
          .void
      applyBlock = (state: State[F], blockId: BlockId) =>
        for {
          body <- fetchBody(blockId)
          // Note: Do not include reward tranaction
          _ <- body.transactionIds.traverse(fetchTransaction(_).flatMap(removeWithExpiration))
        } yield state
      unapplyBlock = (state: State[F], blockId: BlockId) =>
        for {
          body <- fetchBody(blockId)
          // Note: Do not include reward transaction
          _ <- body.transactionIds.traverse(fetchTransaction(_).flatMap(addWithExpiration))
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
      interpreter = new MempoolAlgebra[F] {

        def read(blockId: BlockId): F[MempoolGraph] =
          eventSourcedState
            .useStateAt(blockId)(_.get)

        def add(transactionId: TransactionId): F[Boolean] =
          (fetchTransaction(transactionId).flatMap(addWithExpiration) >> true.pure[F])
            .flatTap(
              Async[F].whenA(_)(
                EitherT(adoptionsTopic.publish1(transactionId))
                  .leftMap(_ => new IllegalStateException("MempoolBroadcaster topic unexpectedly closed"))
                  .rethrowT
              )
            )

        def remove(transactionId: TransactionId): F[Unit] =
          fetchTransaction(transactionId).flatMap(removeWithExpiration)

        def contains(blockId: BlockId, transactionId: TransactionId): F[Boolean] =
          eventSourcedState
            .useStateAt(blockId)(_.get)
            .map(_.transactions.contains(transactionId))

        def adoptions: Topic[F, TransactionId] =
          adoptionsTopic
      }
    } yield (interpreter, eventSourcedState)

}
