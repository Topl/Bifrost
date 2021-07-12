package co.topl.utils.akka

import akka.actor.typed._
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl._
import akka.pattern.StatusReply
import akka.util.Timeout

import java.util.UUID
import scala.concurrent.Future
import scala.util.Try

/**
 * An AsyncRunner takes a synchronous, blocking function, and runs it asynchronously on a special dispatcher
 */
class AsyncRunner private (actorRef: ActorRef[AsyncRunner.Run[_]])(implicit system: ActorSystem[_], timeout: Timeout) {

  def run[T](f: () => T): Future[T] =
    actorRef.askWithStatus(new AsyncRunner.Run(f, _))
}

object AsyncRunner {

  private val behavior: Behavior[Run[_]] =
    Behaviors.receiveMessage[Run[_]] { runnable =>
      runnable.run()
      Behaviors.same
    }

  private val dispatcherSelector = DispatcherSelector.fromConfig("async-runner-actor-dispatcher")

  def apply(
    name:            String = s"async-runner-${UUID.randomUUID()}"
  )(implicit system: ActorSystem[_], timeout: Timeout): AsyncRunner =
    new AsyncRunner(system.systemActorOf(behavior, name, dispatcherSelector))

  private class Run[T](f: () => T, replyTo: ActorRef[StatusReply[T]]) {

    private[AsyncRunner] def run(): Unit = replyTo !
      Try(f()).fold[StatusReply[T]](StatusReply.error[T], StatusReply.success)
  }

}
