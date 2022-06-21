package co.topl.eventtree

import cats._
import cats.data.Chain
import cats.effect._
import cats.effect.std.Semaphore
import cats.implicits._
import co.topl.models._
import co.topl.typeclasses.implicits._

/**
 * Derives/computes/retrieves the State at some eventId
 */
trait EventSourcedState[F[_], State] {

  /**
   * Produces a State at the given Event ID
   */
  def stateAt(eventId: TypedIdentifier): F[State]

  /**
   * Produces a State at the given Event ID and applies the given function to the State.
   *
   * This method is intended to provide thread safety to States that are internally unsafe
   */
  def useStateAt[U](eventId: TypedIdentifier)(f: State => F[U]): F[U]
}

object EventSourcedState {

  /**
   * An EventSourcedState that is assembled from a tree of events instead of a single linear chain
   */
  object OfTree {

    def make[F[_]: Async, State](
      initialState:    F[State],
      initialEventId:  F[TypedIdentifier],
      applyEvent:      (State, TypedIdentifier) => F[State],
      unapplyEvent:    (State, TypedIdentifier) => F[State],
      parentChildTree: ParentChildTree[F, TypedIdentifier]
    ): F[EventSourcedState[F, State]] = for {
      semaphore         <- Semaphore[F](1)
      currentStateRef   <- initialState.flatMap(Ref.of[F, State])
      currentEventIdRef <- initialEventId.flatMap(Ref.of[F, TypedIdentifier])
    } yield new Impl[F, State](
      applyEvent,
      unapplyEvent,
      parentChildTree,
      semaphore,
      currentStateRef,
      currentEventIdRef
    )

    private class Impl[F[_]: Async, State](
      applyEvent:        (State, TypedIdentifier) => F[State],
      unapplyEvent:      (State, TypedIdentifier) => F[State],
      parentChildTree:   ParentChildTree[F, TypedIdentifier],
      semaphore:         Semaphore[F],
      currentStateRef:   Ref[F, State],
      currentEventIdRef: Ref[F, TypedIdentifier]
    ) extends EventSourcedState[F, State] {

      def stateAt(eventId: TypedIdentifier): F[State] =
        useStateAt(eventId)(_.pure[F])

      override def useStateAt[U](eventId: TypedIdentifier)(f: State => F[U]): F[U] =
        semaphore.permit.use(_ =>
          for {
            currentEventId <- currentEventIdRef.get
            state <-
              (currentEventId === eventId)
                .pure[F]
                .ifM(
                  currentStateRef.get,
                  for {
                    ((unapplyChain, applyChain), currentState) <- (
                      parentChildTree.findCommonAncestor(currentEventId, eventId),
                      currentStateRef.get
                    ).tupled
                    stateAtCommonAncestor <- unapplyEvents(unapplyChain.tail, currentState)
                    newState              <- applyEvents(applyChain.tail, stateAtCommonAncestor)
                  } yield newState
                )
            res <- f(state)
          } yield res
        )

      private def unapplyEvents(eventIds: Chain[TypedIdentifier], currentState: State): F[State] =
        eventIds.reverse.foldLeftM(currentState) { case (state, eventId) =>
          for {
            newState <- unapplyEvent(state, eventId)
            _        <- (currentStateRef.set(newState), currentEventIdRef.set(eventId)).tupled
          } yield newState
        }

      private def applyEvents(eventIds: Chain[TypedIdentifier], currentState: State): F[State] =
        eventIds.foldLeftM(currentState) { case (state, eventId) =>
          for {
            newState <- applyEvent(state, eventId)
            _        <- (currentStateRef.set(newState), currentEventIdRef.set(eventId)).tupled
          } yield newState
        }
    }
  }

}
