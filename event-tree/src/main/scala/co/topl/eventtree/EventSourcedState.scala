package co.topl.eventtree

import cats._
import cats.data.{Chain, OptionT}
import cats.effect._
import cats.effect.std.Semaphore
import cats.implicits._
import co.topl.algebras.{Store, StoreReader}
import co.topl.models._
import co.topl.typeclasses.implicits._

/**
 * Derives/computes/retrieves the State at some eventId
 */
trait EventSourcedState[F[_], Event, State] {
  def stateAt(eventId: TypedIdentifier): F[State]
}

object EventSourcedState {

  /**
   * An EventSourcedState that is assembled from a tree of events instead of a single linear chain
   */
  object OfTree {

    def make[F[_]: Async, Event, State, UnapplyEvent](
      initialState:        F[State],
      initialEventId:      F[TypedIdentifier],
      eventAsUnapplyEvent: (Event, State) => F[UnapplyEvent],
      applyEvent:          (State, Event) => F[State],
      unapplyEvent:        (State, UnapplyEvent) => F[State],
      eventStore:          StoreReader[F, TypedIdentifier, Event],
      unapplyEventStore:   Store[F, TypedIdentifier, UnapplyEvent],
      parentChildTree:     ParentChildTree[F, TypedIdentifier]
    ): F[EventSourcedState[F, Event, State]] = for {
      semaphore         <- Semaphore[F](1)
      currentStateRef   <- initialState.flatMap(Ref.of[F, State])
      currentEventIdRef <- initialEventId.flatMap(Ref.of[F, TypedIdentifier])
    } yield new Impl[F, Event, State, UnapplyEvent](
      eventAsUnapplyEvent,
      applyEvent,
      unapplyEvent,
      eventStore,
      unapplyEventStore,
      parentChildTree,
      semaphore,
      currentStateRef,
      currentEventIdRef
    )

    private class Impl[F[_]: Async, Event, State, UnapplyEvent](
      eventAsUnapplyEvent: (Event, State) => F[UnapplyEvent],
      applyEvent:          (State, Event) => F[State],
      unapplyEvent:        (State, UnapplyEvent) => F[State],
      eventStore:          StoreReader[F, TypedIdentifier, Event],
      unapplyEventStore:   Store[F, TypedIdentifier, UnapplyEvent],
      parentChildTree:     ParentChildTree[F, TypedIdentifier],
      semaphore:           Semaphore[F],
      currentStateRef:     Ref[F, State],
      currentEventIdRef:   Ref[F, TypedIdentifier]
    ) extends EventSourcedState[F, Event, State] {

      def stateAt(eventId: TypedIdentifier): F[State] =
        semaphore.permit.use(_ =>
          for {
            currentEventId <- currentEventIdRef.get
            state <- Monad[F].ifElseM(
              Async[F].delay(currentEventId === eventId) -> currentStateRef.get
            )(
              Async[F].defer(
                for {
                  ((unapplyChain, applyChain), currentState) <- (
                    parentChildTree.findCommonAncestor(currentEventId, eventId),
                    currentStateRef.get
                  ).tupled
                  stateAtCommonAncestor <- unapplyEvents(unapplyChain.tail, currentState)
                  newState              <- applyEvents(applyChain.tail, stateAtCommonAncestor)
                } yield newState
              )
            )
          } yield state
        )

      private def unapplyEvents(eventIds: Chain[TypedIdentifier], currentState: State): F[State] =
        eventIds.reverse.foldLeftM(currentState) { case (state, eventId) =>
          for {
            u        <- getUnapply(eventId)
            _        <- unapplyEventStore.remove(eventId)
            newState <- unapplyEvent(state, u)
            _        <- (currentStateRef.set(newState), currentEventIdRef.set(eventId)).tupled
          } yield newState
        }

      private def applyEvents(eventIds: Chain[TypedIdentifier], currentState: State): F[State] =
        eventIds.foldLeftM(currentState) { case (state, eventId) =>
          for {
            event    <- getEvent(eventId)
            unapply  <- eventAsUnapplyEvent(event, state)
            _        <- unapplyEventStore.put(eventId, unapply)
            newState <- applyEvent(state, event)
            _        <- (currentStateRef.set(newState), currentEventIdRef.set(eventId)).tupled
          } yield newState
        }

      private def getEvent(eventId: TypedIdentifier): F[Event] =
        OptionT(eventStore.get(eventId))
          .getOrElseF(MonadThrow[F].raiseError(new NoSuchElementException(show"Apply id=${eventId}")))

      private def getUnapply(eventId: TypedIdentifier): F[UnapplyEvent] =
        OptionT(unapplyEventStore.get(eventId))
          .getOrElseF(MonadThrow[F].raiseError(new NoSuchElementException(show"Unapply id=${eventId}")))
    }
  }
}
