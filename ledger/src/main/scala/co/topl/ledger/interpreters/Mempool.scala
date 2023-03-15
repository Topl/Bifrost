package co.topl.ledger.interpreters

import cats.{Applicative, MonadThrow}
import cats.data.OptionT
import cats.effect.kernel.Spawn
import cats.effect.{Async, Deferred, Fiber, Ref, Resource, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.brambl.models.Identifier
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.Slot
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._

// TODO: Non-minting nodes?
object Mempool {
  private case class MempoolEntry[F[_]](expirationFiber: Fiber[F, _, _], inputBoxIds: Set[TransactionOutputAddress])
  private type State[F[_]] = Ref[F, Map[Identifier.IoTransaction32, MempoolEntry[F]]]

  /**
   * @param defaultExpirationLimit The maximum number of slots in the future allowed by a transaction expiration
   * @param duplicateSpenderSlotLifetime The maximum number of slots that a Transaction should remain in a mempool after detecting
   *                                     that it double-spends a Box
   * @return
   */
  def make[F[_]: Async](
    currentBlockId:               F[BlockId],
    fetchBlockBody:               BlockId => F[BlockBody],
    fetchTransaction:             Identifier.IoTransaction32 => F[IoTransaction],
    parentChildTree:              ParentChildTree[F, BlockId],
    currentEventChanged:          BlockId => F[Unit],
    clock:                        ClockAlgebra[F],
    onExpiration:                 Identifier.IoTransaction32 => F[Unit],
    defaultExpirationLimit:       Long,
    duplicateSpenderSlotLifetime: Long
  ): Resource[F, MempoolAlgebra[F]] =
    for {
      state <- Resource.make(Ref.of(Map.empty[Identifier.IoTransaction32, MempoolEntry[F]]))(
        _.get.flatMap(_.values.toList.traverse(_.expirationFiber.cancel).void)
      )
      // A function which inserts a transaction into the mempool and schedules its expiration using a Fiber
      addTransaction = (transaction: IoTransaction, expirationSlot: Slot) =>
        for {
          expirationTask <- Async[F].delay(
            clock.delayedUntilSlot(expirationSlot) >> Sync[F].defer(
              (
                state.update(_ - transaction.id),
                onExpiration(transaction.id)
              ).tupled
            )
          )
          expirationFiber <- Spawn[F].start(expirationTask)
          entry = MempoolEntry(expirationFiber, transaction.inputs.map(_.address).toSet)
          _ <- state.update(_.updated(transaction.id, entry))
        } yield ()
      // A function which inserts a transaction into the mempool using the limit specified in the transaction
      addTransactionWithDefaultExpiration = (transaction: IoTransaction) =>
        for {
          currentSlot <- clock.globalSlot
          targetSlot = transaction.datum.event.schedule.max.min(currentSlot + defaultExpirationLimit)
          _ <- addTransaction(transaction, targetSlot)
        } yield ()
      applyBlock = (state: State[F], blockId: BlockId) =>
        for {
          blockBody         <- fetchBlockBody(blockId).map(_.transactionIds.toList)
          blockTransactions <- blockBody.traverse(fetchTransaction)
          blockInputIds = blockTransactions.flatMap(_.inputs.map(_.address)).toSet
          currentEntries <- state.get
          // First, cancel the scheduled expirations for the transactions associated with the block
          expirationsToCancel = blockBody.flatMap(currentEntries.get)
          _ <- expirationsToCancel.traverse(_.expirationFiber.cancel)
          // Next, calculate which of the remaining Mempool transactions attempt to spend an input that is spent
          // by the current block
          duplicateSpenderMempoolEntries = (currentEntries -- blockBody).filter { case (_, entry) =>
            // Is there overlap between the inputs of the block and the inputs in this current entry/transaction?
            // If so, consider this entry to be a duplicate spender
            entry.inputBoxIds.intersect(blockInputIds).nonEmpty
          }
          globalSlot <- clock.globalSlot
          // Now, reschedule the expiration for the duplicate spender transactions
          _ <- duplicateSpenderMempoolEntries.toList.traverse { case (id, entry) =>
            // Cancel the current expiration fiber
            entry.expirationFiber.cancel >>
            // And create a new one, but with a much shorter expiration window
            fetchTransaction(id).flatMap(addTransaction(_, globalSlot + duplicateSpenderSlotLifetime))
          }
          _ <- state.update(_ -- blockBody)
        } yield state
      unapplyBlock = (state: State[F], blockId: BlockId) =>
        fetchBlockBody(blockId)
          .map(_.transactionIds.toList)
          .flatMap(_.traverse(fetchTransaction(_).flatMap(addTransactionWithDefaultExpiration)))
          .as(state)
      eventSourcedState <- Resource.eval(
        EventSourcedState.OfTree.make[F, State[F], BlockId](
          state.pure[F],
          currentBlockId,
          applyEvent = applyBlock,
          unapplyEvent = unapplyBlock,
          parentChildTree,
          currentEventChanged
        )
      )
      finalizing <- Resource.make(Deferred[F, Unit])(_.complete(()).void)
    } yield new MempoolAlgebra[F] {

      def read(blockId: BlockId): F[Set[Identifier.IoTransaction32]] =
        whenNotTerminated(
          eventSourcedState.stateAt(blockId).flatMap(_.get).map(_.keySet)
        )

      // TODO: Check for double-spends along current canonical chain?
      def add(transactionId: Identifier.IoTransaction32): F[Unit] =
        whenNotTerminated(
          fetchTransaction(transactionId).flatMap(addTransactionWithDefaultExpiration)
        )

      def remove(transactionId: Identifier.IoTransaction32): F[Unit] =
        whenNotTerminated(
          state
            .getAndUpdate(_ - transactionId)
            .flatTap(entries =>
              OptionT.fromOption[F](entries.get(transactionId)).foldF(Applicative[F].unit)(_.expirationFiber.cancel)
            )
            .void
        )

      /**
       * Verifies that the mempool is not terminated/finalizing before running the provided action.  If the mempool
       * is terminated, the call will immediately fail
       */
      private def whenNotTerminated[T](f: => F[T]): F[T] =
        OptionT(finalizing.tryGet)
          .foldF(f)(_ => MonadThrow[F].raiseError(new IllegalStateException("Mempool Terminated")))
    }
}
