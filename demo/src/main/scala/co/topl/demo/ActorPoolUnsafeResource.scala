package co.topl.demo

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, PostStop}
import akka.util.Timeout
import cats.data.Chain
import cats.effect.{Async, Sync}
import co.topl.algebras.UnsafeResource

import java.util.UUID

object ActorPoolUnsafeResource {

  object Eval {

    def make[F[_]: Async, T](init: => T, cleanup: T => Unit)(implicit
      system:                      ActorSystem[_],
      askTimeout:                  Timeout
    ): F[UnsafeResource[F, T]] =
      Sync[F].delay {
        val parent = system.systemActorOf(
          ParentActor[T](init, cleanup, Chain.empty),
          "ActorPoolUnsharedResource" + UUID.randomUUID().toString
        )

        new UnsafeResource[F, T] {
          import akka.actor.typed.scaladsl.AskPattern._
          def use[Res](f: T => Res): F[Res] =
            Async[F].fromFuture(
              Sync[F].delay(
                parent.ask(
                  ParentActor.Use(f, _)
                )
              )
            )
        }
      }
  }

  private object ParentActor {
    sealed abstract class ReceivableMessage[T]

    case class Use[T, Res](f: T => Res, replyTo: ActorRef[Res]) extends ReceivableMessage[T] {

      def run(child: ActorRef[ChildActor.ReceivableMessage[T]]): Unit =
        child.tell(ChildActor.Use(f, replyTo))
    }
    case class Ready[T](child: ActorRef[ChildActor.ReceivableMessage[T]]) extends ReceivableMessage[T]

    def apply[T](
      init:             => T,
      cleanup:          T => Unit,
      inactiveChildren: Chain[ActorRef[ChildActor.ReceivableMessage[T]]]
    ): Behavior[ReceivableMessage[T]] =
      Behaviors.receive {
        case (ctx, u @ Use(_, _)) =>
          val (remaining, child) =
            inactiveChildren.initLast.getOrElse(
              (Chain.empty, ctx.spawnAnonymous(ChildActor(init, cleanup, ctx.self)))
            )
          u.run(child)
          apply(init, cleanup, remaining)

        case (_, Ready(child)) =>
          apply(init, cleanup, inactiveChildren :+ child)
      }
  }

  private object ChildActor {
    sealed abstract class ReceivableMessage[T]

    case class Use[T, Res](f: T => Res, replyTo: ActorRef[Res]) extends ReceivableMessage[T] {
      def run(t: T): Unit = replyTo.tell(f(t))
    }

    def apply[T](
      value:   T,
      cleanup: T => Unit,
      parent:  ActorRef[ParentActor.ReceivableMessage[T]]
    ): Behavior[ReceivableMessage[T]] =
      Behaviors
        .receive[ReceivableMessage[T]] { case (ctx, u @ Use(_, _)) =>
          u.run(value)
          parent.tell(ParentActor.Ready(ctx.self))
          Behaviors.same
        }
        .receiveSignal { case (_, PostStop) =>
          cleanup(value)
          Behaviors.same
        }
  }
}
