package co.topl.loadtesting

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * An actor which loops indefinitely using a given function which inputs and outputs a state.
 */
object LoopActor {

  /**
   * Instantiates a looping actor `Behavior` for a given `State` type.
   * @param behavior the behavior to run during each loop
   * @param onFailure the action to take on a failure
   * @tparam State the state tracked between each loop
   * @return a looping actor behavior
   */
  def apply[State](
    behavior:  (State, ActorContext[State]) => Future[State],
    onFailure: Throwable => State
  ): Behavior[State] =
    Behaviors.receive { (context, message) =>
      message match {
        case state =>
          // send self the result of running the behavior
          context.pipeToSelf(behavior(state, context)) {
            case Success(state) => state
            case Failure(err)   => onFailure(err)
          }
          Behaviors.same
      }
    }
}
