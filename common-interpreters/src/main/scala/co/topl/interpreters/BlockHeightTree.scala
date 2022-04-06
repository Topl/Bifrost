package co.topl.interpreters

import cats.MonadThrow
import cats.data.OptionT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.{Store, StoreReader}
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models.{SlotData, TypedIdentifier}
import co.topl.typeclasses.implicits._

object BlockHeightTree {

  type State[F[_]] = (Long => F[Option[TypedIdentifier]])

  def make[F[_]: Async](
    store:          Store[F, Long, TypedIdentifier],
    initialEventId: TypedIdentifier,
    slotDataStore:  StoreReader[F, TypedIdentifier, SlotData],
    unapplyStore:   Store[F, TypedIdentifier, Long],
    blockTree:      ParentChildTree[F, TypedIdentifier]
  ): F[EventSourcedState[F, TypedIdentifier, State[F]]] =
    EventSourcedState.OfTree.make[F, TypedIdentifier, State[F], Long](
      Async[F].delay(store.get),
      initialEventId = initialEventId.pure[F],
      (id, _) => heightOf(slotDataStore)(id),
      (state, id) => heightOf(slotDataStore)(id).flatTap(store.put(_, id)).as(state),
      (state, height) => store.remove(height).as(state),
      slotDataStore.mapRead(identity, _.slotId.blockId),
      unapplyStore,
      blockTree
    )

  private def heightOf[F[_]: MonadThrow](store: StoreReader[F, TypedIdentifier, SlotData])(id: TypedIdentifier) =
    OptionT(store.get(id))
      .getOrElseF(MonadThrow[F].raiseError(new IllegalStateException(show"Local slot data not found id=$id")))
      .map(_.height)

}
