package co.topl.interpreters

import cats.effect.kernel.Async
import co.topl.algebras.Store
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.node.models.BlockBody
import cats.implicits._
import co.topl.typeclasses.implicits._

object TxIdToBlockIdTree {
  type State[F[_]] = Store[F, TransactionId, BlockId]

  def make[F[_]: Async](
    currentBlockId: F[BlockId],
    blockIdToBody:  BlockId => F[Option[BlockBody]],
    initialState:   State[F],
    blockTree:      ParentChildTree[F, BlockId]
  ): F[EventSourcedState[F, State[F], BlockId]] =
    EventSourcedState.OfTree.make[F, State[F], BlockId](
      initialState = initialState.pure[F],
      initialEventId = currentBlockId,
      applyEvent = (storage: State[F], blockId: BlockId) =>
        blockIdToBody(blockId).flatMap {
          case Some(body) => body.transactionIds.traverse(storage.put(_, blockId)) >> storage.pure[F]
          case None       => storage.pure[F]
        },
      unapplyEvent = (storage: State[F], id: BlockId) =>
        blockIdToBody(id).flatMap {
          case Some(body) => body.transactionIds.traverse(txId => storage.remove(txId)) >> storage.pure[F]
          case None       => storage.pure[F]
        },
      parentChildTree = blockTree,
      _ => Async[F].unit
    )
}
