package co.topl.ledger.interpreters

import cats.effect.kernel.Spawn
import cats.effect.{Async, Fiber, Ref}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models._
import co.topl.typeclasses.implicits._

// TODO: Non-minting nodes?
object Mempool {
  private case class MempoolEntry[F[_]](expirationFiber: Fiber[F, _, _], inputBoxIds: Set[Box.Id])
  private type State[F[_]] = Ref[F, Map[TypedIdentifier, MempoolEntry[F]]]

  /**
   * @param defaultExpirationLimit The maximum number of slots in the future allowed by a transaction expiration
   * @param duplicateSpenderSlotLifetime The maximum number of slots that a Transaction should remain in a mempool after detecting
   *                                     that it double-spends a Box
   * @return
   */
  def make[F[_]: Async: Spawn](
    currentBlockId:               F[TypedIdentifier],
    fetchBlockBody:               TypedIdentifier => F[BlockBodyV2],
    fetchTransaction:             TypedIdentifier => F[Transaction],
    parentChildTree:              ParentChildTree[F, TypedIdentifier],
    clock:                        ClockAlgebra[F],
    defaultExpirationLimit:       Long,
    duplicateSpenderSlotLifetime: Long
  ): F[MempoolAlgebra[F]] =
    for {
      state <- Ref.of(Map.empty[TypedIdentifier, MempoolEntry[F]])
      // A function which inserts a transaction into the mempool and schedules its expiration using a Fiber
      addTransaction = (transaction: Transaction, expirationSlot: Slot) =>
        for {
          expirationTask <-
            //
            Async[F].delay(clock.delayedUntilSlot(expirationSlot) >> state.update(_ - transaction.id))
          expirationFiber <- Spawn[F].start(expirationTask)
          entry = MempoolEntry(expirationFiber, transaction.inputs.map(_.boxId).toList.toSet)
          _ <- state.update(_.updated(transaction.id, entry))
        } yield ()
      // A function which inserts a transaction into the mempool using the limit specified in the transaction
      addTransactionWithDefaultDelay = (transaction: Transaction) =>
        for {
          currentSlot <- clock.globalSlot
          targetSlot = transaction.chronology.maximumSlot.min(currentSlot + defaultExpirationLimit)
          _ <- addTransaction(transaction, targetSlot)
        } yield ()
      applyBlock = (state: State[F], blockId: TypedIdentifier) =>
        for {
          blockBody         <- fetchBlockBody(blockId)
          blockTransactions <- blockBody.traverse(fetchTransaction)
          blockInputIds = blockTransactions.flatMap(_.inputs.map(_.boxId).toList).toSet
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
      unapplyBlock = (state: State[F], blockId: TypedIdentifier) =>
        fetchBlockBody(blockId)
          .flatMap(_.traverse(fetchTransaction(_).flatMap(addTransactionWithDefaultDelay)))
          .as(state)
      eventSourcedState <- EventSourcedState.OfTree.make[F, State[F]](
        state.pure[F],
        currentBlockId,
        applyEvent = applyBlock,
        unapplyEvent = unapplyBlock,
        parentChildTree
      )
    } yield new MempoolAlgebra[F] {

      def read(blockId: TypedIdentifier): F[Set[TypedIdentifier]] =
        eventSourcedState.stateAt(blockId).flatMap(_.get).map(_.keySet)

      def add(transactionId: TypedIdentifier): F[Unit] =
        fetchTransaction(transactionId).flatMap(addTransactionWithDefaultDelay)

      def remove(transactionId: TypedIdentifier): F[Unit] =
        state.update(_ - transactionId)
    }
}