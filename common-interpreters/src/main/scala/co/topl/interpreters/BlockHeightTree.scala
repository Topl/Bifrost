package co.topl.interpreters

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.{Store, StoreReader}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models.TypedIdentifier
import co.topl.consensus.models.SlotData
import co.topl.typeclasses.implicits._

object BlockHeightTree {

  type State[F[_]] = Long => F[Option[TypedIdentifier]]

  def make[F[_]: Async](
    store:               Store[F, Long, TypedIdentifier],
    initialEventId:      F[TypedIdentifier],
    slotDataStore:       StoreReader[F, TypedIdentifier, SlotData],
    blockTree:           ParentChildTree[F, TypedIdentifier],
    currentEventChanged: TypedIdentifier => F[Unit]
  ): F[EventSourcedState[F, State[F], TypedIdentifier]] = {
    val heightStore = slotDataStore.mapRead[TypedIdentifier, Long](identity, _.height)
    EventSourcedState.OfTree.make[F, State[F], TypedIdentifier](
      Async[F].delay(store.get),
      initialEventId = initialEventId,
      (state, id) => heightStore.getOrRaise(id).flatTap(store.put(_, id)).as(state),
      (state, id) => heightStore.getOrRaise(id).flatTap(store.remove).as(state),
      blockTree,
      currentEventChanged
    )
  }

}
