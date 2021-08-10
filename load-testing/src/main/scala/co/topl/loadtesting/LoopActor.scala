package co.topl.loadtesting

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * An actor which loops indefinitely.
 */
object LoopActor {

  def apply[State](
    behavior:  (State, ActorContext[State]) => Future[State],
    onFailure: Throwable => State
  ): Behavior[State] =
    Behaviors.receive { (context, message) =>
      message match {
        case state: State =>
          context.pipeToSelf(behavior(state, context)) {
            case Success(state) => state
            case Failure(err)    => onFailure(err)
          }
          Behaviors.same
      }
    }
}
