package co.topl.interpreters

import akka.actor.typed._
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.pattern.StatusReply
import akka.util.Timeout
import cats.data.Chain
import cats.effect.{Async, Sync}
import co.topl.algebras.UnsafeResource
import co.topl.catsakka.FToFuture

import java.util.UUID

/**
 * Interprets the `UnsafeResource` algebra using a pool of actors.  When a Resource is needed, an actor is created to
 * own the Resource and handle commands to use it.  If another request arrives before the first actor completes, a second
 * actor is created.  When the first actor finishes, it is into the pool of available workers.  Actors can then
 * be re-used.
 */
object ActorPoolUnsafeResource {

  object Eval {

    def make[F[_]: Async: FToFuture, T](init: => T, cleanup: T => Unit)(implicit
      system:                                 ActorSystem[_],
      askTimeout:                             Timeout
    ): F[UnsafeResource[F, T]] =
      Sync[F].delay {
        implicit val scheduler: Scheduler = system.scheduler
        val parent = system.systemActorOf(
          PoolManagerActor[T](init, cleanup, Chain.empty),
          "ActorPoolUnsharedResource" + UUID.randomUUID().toString
        )

        // TODO: Release resources after inactivity period
        new UnsafeResource[F, T] {
          import CatsActor._
          def use[Res](f: T => F[Res]): F[Res] =
            parent.askMWithStatus[F, Res](PoolManagerActor.Use(f, _))
        }
      }
  }

  /**
   * Maintains a set of available worker actors.  Forwards incoming requests to the first available worker.
   */
  private object PoolManagerActor {
    sealed abstract class ReceivableMessage[T]

    case class Use[F[_]: FToFuture, T, Res](f: T => F[Res], replyTo: ActorRef[StatusReply[Res]])
        extends ReceivableMessage[T] {

      def run(child: ActorRef[SingleResourceOwner.ReceivableMessage[T]]): Unit =
        child.tell(SingleResourceOwner.Use(f, replyTo))
    }
    case class Ready[T](child: ActorRef[SingleResourceOwner.ReceivableMessage[T]]) extends ReceivableMessage[T]

    def apply[T](
      init:             => T,
      cleanup:          T => Unit,
      inactiveChildren: Chain[ActorRef[SingleResourceOwner.ReceivableMessage[T]]]
    ): Behavior[ReceivableMessage[T]] =
      Behaviors.receive {
        case (ctx, u @ Use(_, _)) =>
          val (remaining, child) =
            inactiveChildren.initLast.getOrElse(
              (Chain.empty, ctx.spawnAnonymous(SingleResourceOwner(init, cleanup, ctx.self)))
            )
          u.run(child)
          apply(init, cleanup, remaining)

        case (_, Ready(child)) =>
          apply(init, cleanup, inactiveChildren :+ child)
      }
  }

  /**
   * Owns an individual resource.  Handles commands to use the resource.  Notifies the pool upon completion.
   */
  private object SingleResourceOwner {
    sealed abstract class ReceivableMessage[T]

    case class Use[F[_]: FToFuture, T, Res](f: T => F[Res], replyTo: ActorRef[StatusReply[Res]])
        extends ReceivableMessage[T] {

      def run(t: T, ctx: ActorContext[ReceivableMessage[T]]): Unit =
        ctx.pipeToSelf(implicitly[FToFuture[F]].apply(f(t)))(e =>
          Complete(
            e.fold(StatusReply.error(_), StatusReply.success),
            replyTo
          )
        )
    }

    case class Complete[T, Res](result: StatusReply[Res], replyTo: ActorRef[StatusReply[Res]])
        extends ReceivableMessage[T] {

      def run(): Unit =
        replyTo.tell(result)
    }

    def apply[T](
      value:   T,
      cleanup: T => Unit,
      parent:  ActorRef[PoolManagerActor.ReceivableMessage[T]]
    ): Behavior[ReceivableMessage[T]] =
      Behaviors
        .receive[ReceivableMessage[T]] {
          case (ctx, u @ Use(_, _)) =>
            u.run(value, ctx)
            Behaviors.same
          case (ctx, u @ Complete(_, _)) =>
            u.run()
            parent.tell(PoolManagerActor.Ready(ctx.self))
            Behaviors.same
        }
        .receiveSignal { case (_, PostStop) =>
          cleanup(value)
          Behaviors.same
        }
  }
}

object CatsActor {

  implicit class ActorRefOps[T](ref: ActorRef[T]) {
    import akka.actor.typed.scaladsl.AskPattern._

    def askM[F[_]: Async, Res](messageF: ActorRef[Res] => T)(implicit timeout: Timeout, scheduler: Scheduler): F[Res] =
      Async[F].fromFuture(Async[F].delay(ref.ask(messageF)))

    def askMWithStatus[F[_]: Async, Res](
      messageF:         ActorRef[StatusReply[Res]] => T
    )(implicit timeout: Timeout, scheduler: Scheduler): F[Res] =
      Async[F].fromFuture(Async[F].delay(ref.askWithStatus(messageF)))
  }
}
