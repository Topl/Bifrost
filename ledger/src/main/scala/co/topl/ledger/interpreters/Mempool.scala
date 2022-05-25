package co.topl.ledger.interpreters

import cats.effect.kernel.Spawn
import cats.effect.{Async, Fiber, Ref}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.models.{BlockBodyV2, Box, Slot, Transaction, TypedIdentifier}

// TODO: Non-minting nodes?
object Mempool {

  /**
   * @param defaultExpirationLimit The maximum number of slots in the future allowed by a transaction expiration
   * @param duplicateSpenderSlotLifetime The maximum number of slots that a Transaction should remain in a mempool after detecting
   *                                     that it double-spends a Box
   * @return
   */
  def make[F[_]: Async: Spawn](
    currentBlockId:               F[TypedIdentifier],
    fetchBlockBody:               TypedIdentifier => F[BlockBodyV2],
    parentChildTree:              ParentChildTree[F, TypedIdentifier],
    getTransaction:               TypedIdentifier => F[Transaction],
    clock:                        ClockAlgebra[F],
    defaultExpirationLimit:       Long,
    duplicateSpenderSlotLifetime: F[Long]
  ): F[MempoolAlgebra[F]] = {
    case class MempoolEntry(expirationFiber: Fiber[F, _, _], inputBoxIds: Set[Box.Id])
    type State = Ref[F, Map[TypedIdentifier, MempoolEntry]]
    for {
      state <- Ref.of(Map.empty[TypedIdentifier, MempoolEntry])
      // A function which inserts a transaction into the mempool and schedules its expiration using a Fiber
      addTransaction = (transactionId: TypedIdentifier, expirationSlot: Slot) =>
        for {
          transaction <- getTransaction(transactionId)
          expirationTask = clock.delayedUntilSlot(expirationSlot) >> state.update(_ - transactionId)
          expirationFiber <- Spawn[F].start(expirationTask)
          entry = MempoolEntry(expirationFiber, transaction.inputs.map(_.boxId).toList.toSet)
          _ <- state.update(_.updated(transactionId, entry))
        } yield ()
      // A function which inserts a transaction into the mempool using the limit specified in the transaction
      addTransactionWithDefaultDelay = (transactionId: TypedIdentifier) =>
        for {
          transaction <- getTransaction(transactionId)
          currentSlot <- clock.globalSlot
          targetSlot = transaction.chronology.maximumSlot.max(currentSlot + defaultExpirationLimit)
          _ <- addTransaction(transactionId, targetSlot)
        } yield ()
      applyBlock = (state: State, blockId: TypedIdentifier) =>
        for {
          blockBody         <- fetchBlockBody(blockId)
          blockTransactions <- blockBody.traverse(getTransaction)
          blockInputIds = blockTransactions.flatMap(_.inputs.map(_.boxId).toList).toSet
          currentEntries <- state.get
          duplicateSpenderMempoolEntries = (currentEntries -- blockBody).filter { case (_, entry) =>
            // Is there overlap between the inputs of the block and the inputs in this current entry?  If so,
            // consider this entry to be a duplicate spender
            entry.inputBoxIds.intersect(blockInputIds).nonEmpty
          }
          expirationsToCancel = blockBody.map(currentEntries)
          _          <- expirationsToCancel.traverse(_.expirationFiber.cancel)
          globalSlot <- clock.globalSlot
          eagerness  <- duplicateSpenderSlotLifetime
          _ <- duplicateSpenderMempoolEntries.toList.traverse { case (id, entry) =>
            entry.expirationFiber.cancel >> addTransaction(id, globalSlot + eagerness)
          }
          _ <- state.update(_ -- blockBody)
        } yield state
      unapplyBlock = (state: State, blockId: TypedIdentifier) =>
        fetchBlockBody(blockId).flatMap(_.traverse(addTransactionWithDefaultDelay)).as(state)
      eventSourcedState <- EventSourcedState.OfTree.make[F, State](
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
        addTransactionWithDefaultDelay(transactionId)

      def remove(transactionId: TypedIdentifier): F[Unit] =
        state.update(_ - transactionId)
    }
  }
}
