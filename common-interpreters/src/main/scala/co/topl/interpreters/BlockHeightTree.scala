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

  type State[F[_]] = Long => F[Option[TypedIdentifier]]

  def make[F[_]: Async](
    store:          Store[F, Long, TypedIdentifier],
    initialEventId: TypedIdentifier,
    slotDataStore:  StoreReader[F, TypedIdentifier, SlotData],
    unapplyStore:   Store[F, TypedIdentifier, Long],
    blockTree:      ParentChildTree[F, TypedIdentifier]
  ): F[EventSourcedState[F, State[F]]] = {
    val heightStore = slotDataStore.mapRead[TypedIdentifier, Long](identity, _.height)
    val getHeight = heightOf[F](heightStore) _
    EventSourcedState.OfTree.make[F, State[F]](
      Async[F].delay(store.get),
      initialEventId = initialEventId.pure[F],
      (state, id) => getHeight(id).flatTap(store.put(_, id)).as(state),
      (state, id) => getHeight(id).flatMap(height => store.remove(height).as(state)),
      blockTree
    )
  }

  private def heightOf[F[_]: MonadThrow](store: StoreReader[F, TypedIdentifier, Long])(id: TypedIdentifier) =
    OptionT(store.get(id))
      .getOrElseF(MonadThrow[F].raiseError(new IllegalStateException(show"Local slot data not found id=$id")))

}
