package co.topl.eventtree

import cats.Eq
import cats.data.Chain
import cats.effect._
import cats.effect.std.Semaphore
import cats.implicits._

/**
 * Derives/computes/retrieves the State at some eventId
 */
trait EventSourcedState[F[_], State, Id] {

  /**
   * Produces a State at the given Event ID
   */
  def stateAt(eventId: Id): F[State]

  /**
   * Produces a State at the given Event ID and applies the given function to the State.
   *
   * This method is intended to provide thread safety to States that are internally unsafe
   */
  def useStateAt[U](eventId: Id)(f: State => F[U]): F[U]
}

object EventSourcedState {

  /**
   * An EventSourcedState that is assembled from a tree of events instead of a single linear chain
   */
  object OfTree {

    def make[F[_]: Async, State, Id: Eq](
      initialState:        F[State],
      initialEventId:      F[Id],
      applyEvent:          (State, Id) => F[State],
      unapplyEvent:        (State, Id) => F[State],
      parentChildTree:     ParentChildTree[F, Id],
      currentEventChanged: Id => F[Unit]
    ): F[EventSourcedState[F, State, Id]] = for {
      semaphore         <- Semaphore[F](1)
      currentStateRef   <- initialState.flatMap(Ref.of[F, State])
      currentEventIdRef <- initialEventId.flatMap(Ref.of[F, Id])
    } yield new Impl[F, State, Id](
      applyEvent,
      unapplyEvent,
      parentChildTree,
      semaphore,
      currentStateRef,
      currentEventIdRef,
      currentEventChanged
    )

    private class Impl[F[_]: Async, State, Id: Eq](
      applyEvent:          (State, Id) => F[State],
      unapplyEvent:        (State, Id) => F[State],
      parentChildTree:     ParentChildTree[F, Id],
      semaphore:           Semaphore[F],
      currentStateRef:     Ref[F, State],
      currentEventIdRef:   Ref[F, Id],
      currentEventChanged: Id => F[Unit]
    ) extends EventSourcedState[F, State, Id] {

      def stateAt(eventId: Id): F[State] =
        useStateAt(eventId)(_.pure[F])

      override def useStateAt[U](eventId: Id)(f: State => F[U]): F[U] =
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
                    stateAtCommonAncestor <- unapplyEvents(unapplyChain.tail, currentState, unapplyChain.head)
                    newState              <- applyEvents(applyChain.tail, stateAtCommonAncestor)
                  } yield newState
                )
            res <- f(state)
          } yield res
        )

      private def unapplyEvents(
        eventIds:     Chain[Id],
        currentState: State,
        newEventId:   Id
      ): F[State] =
        eventIds.zipWithIndex.reverse.foldLeftM(currentState) { case (state, (eventId, index)) =>
          Async[F].uncancelable(_ =>
            for {
              newState <- unapplyEvent(state, eventId)
              nextEventId = eventIds.get(index - 1).getOrElse(newEventId)
              _ <- (
                currentStateRef.set(newState),
                currentEventIdRef.set(nextEventId),
                currentEventChanged(nextEventId)
              ).tupled
            } yield newState
          )
        }

      private def applyEvents(eventIds: Chain[Id], currentState: State): F[State] =
        eventIds.foldLeftM(currentState) { case (state, eventId) =>
          Async[F].uncancelable(_ =>
            for {
              newState <- applyEvent(state, eventId)
              _ <- (currentStateRef.set(newState), currentEventIdRef.set(eventId), currentEventChanged(eventId)).tupled
            } yield newState
          )
        }
    }
  }

}
