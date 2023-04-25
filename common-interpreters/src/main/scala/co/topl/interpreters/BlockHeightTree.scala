package co.topl.interpreters

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.{Store, StoreReader}
import co.topl.consensus.models.BlockId
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.consensus.models.SlotData
import co.topl.typeclasses.implicits._

object BlockHeightTree {

  type State[F[_]] = Long => F[Option[BlockId]]

  def make[F[_]: Async](
    store:               Store[F, Long, BlockId],
    initialEventId:      F[BlockId],
    slotDataStore:       StoreReader[F, BlockId, SlotData],
    blockTree:           ParentChildTree[F, BlockId],
    currentEventChanged: BlockId => F[Unit]
  ): F[EventSourcedState[F, State[F], BlockId]] = {
    val heightStore = slotDataStore.mapRead[BlockId, Long](identity, _.height)
    EventSourcedState.OfTree.make[F, State[F], BlockId](
      Async[F].delay(store.get),
      initialEventId = initialEventId,
      (state, id) => heightStore.getOrRaise(id).flatTap(store.put(_, id)).as(state),
      (state, id) => heightStore.getOrRaise(id).flatTap(store.remove).as(state),
      blockTree,
      currentEventChanged
    )
  }

}
