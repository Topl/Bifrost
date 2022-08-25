package co.topl.catsakka

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import cats.data.OptionT
import cats.effect.Fiber
import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.implicits._

import java.util.UUID

trait Iterative[F[_], S, E] {
  def improve(currentState: S): F[(S, E)]
}

object Iterative {

  def run[F[_]: Async, S, E](init: F[S])(iterative: Iterative[F, S, E]): F[() => F[E]] =
    Queue
      .bounded[F, Unit](1)
      .flatMap(queue =>
        for {
          v <- init
          fiber <- Async[F].start(
            v.tailRecM(s =>
              iterative
                .improve(s)
                .flatMap { case (nextState, currentBest) =>
                  OptionT(queue.tryTake)
                    .fold[Either[S, E]](Left(nextState))(_ => Right(currentBest))
                }
            )
          )
        } yield () => queue.offer(()) >> fiber.joinWith(Async[F].raiseError(new IllegalStateException("Cancelled")))
      )

  def runActor[F[_]: Async: FToFuture, S, E](init: F[S])(iterative: Iterative[F, S, E])(implicit
    system:                                        ActorSystem[_]
  ): F[() => F[E]] = {
    import akka.actor.typed.scaladsl.AskPattern._
    import scala.concurrent.duration._
    // TODO: Parameter
    implicit val timeout: Timeout = Timeout(10.seconds)
    Async[F]
      .delay(system.systemActorOf(IterativeActor[F, S, E](init)(iterative), UUID.randomUUID().toString))
      .map(actor => () => Async[F].fromFuture(Async[F].delay(actor.ask(IterativeActor.Messages.Stop[E](_)))))
  }

}

object IterativeActor {
  sealed abstract class Message[+F[_], +S, +E]

  object Messages {
    case class Initialize[S](s: S) extends Message[Nothing, S, Nothing]
    case object Tick extends Message[Nothing, Nothing, Nothing]
    case class FiberStarted[F[_], S, E](fiber: Fiber[F, Throwable, TickComplete[S, E]]) extends Message[F, S, E]
    case object FiberCancelled extends Message[Nothing, Nothing, Nothing]
    case class TickComplete[S, E](s: S, e: E) extends Message[Nothing, S, E]
    case class Stop[E](replyTo: ActorRef[E]) extends Message[Nothing, Nothing, E]
  }

  def apply[F[_]: Async: FToFuture, S, E](init: F[S])(iterative: Iterative[F, S, E]): Behavior[Message[F, S, E]] = {
    def initialized(
      currentState: S,
      currentBest:  Option[E],
      currentFiber: Option[Fiber[F, Throwable, Messages.TickComplete[S, E]]]
    ): Behaviors.Receive[Message[F, S, E]] =
      Behaviors.receivePartial[Message[F, S, E]] {
        case (ctx, Messages.Tick) =>
          launchTick(ctx, currentState)
          Behaviors.same
        case (ctx, Messages.FiberStarted(fiber)) =>
          ctx.pipeToSelf(
            implicitly[FToFuture[F]].apply(
              fiber.joinWith(Async[F].raiseError(CancelException))
            )
          )(_.fold(_ => ???, identity))
          Behaviors.same
        case (ctx, Messages.TickComplete(s, e)) =>
          ctx.self.tell(Messages.Tick)
          initialized(s, e.some, None)
        case (ctx, Messages.Stop(replyTo: ActorRef[E])) =>
          currentBest match {
            case Some(best: E) =>
              replyTo.tell(best)
              currentFiber.foreach(f => implicitly[FToFuture[F]].apply(f.cancel))
              awaitingLingeringTick
            case _ =>
              awaitingFirstValue(replyTo)
          }
      }

    def launchTick(ctx: ActorContext[Message[F, S, E]], currentState: S): Unit =
      ctx.pipeToSelf(
        implicitly[FToFuture[F]].apply(
          Async[F].start(
            iterative.improve(currentState).map { case (s, e) => Messages.TickComplete(s, e) }
          )
        )
      )(_.fold(_ => ???, fiber => Messages.FiberStarted(fiber)))

    def awaitingFirstValue(replyTo: ActorRef[E]) =
      Behaviors.receivePartial[Message[F, S, E]] {
        case (_, Messages.TickComplete(_, e)) =>
          replyTo.tell(e)
          Behaviors.stopped
        case (ctx, Messages.FiberStarted(fiber)) =>
          ctx.pipeToSelf(
            implicitly[FToFuture[F]].apply(
              fiber.joinWith(Async[F].raiseError(CancelException))
            )
          )(_.fold(_ => ???, identity))
          Behaviors.same
      }

    def awaitingLingeringTick =
      Behaviors.receiveMessagePartial[Message[F, S, E]] {
        case Messages.FiberCancelled =>
          Behaviors.stopped
        case Messages.TickComplete(_, _) =>
          Behaviors.stopped
      }
    val uninitialized =
      Behaviors.withStash[Message[F, S, E]](1)(stash =>
        Behaviors.receivePartial[Message[F, S, E]] {
          case (ctx, Messages.Initialize(initialValue)) =>
            launchTick(ctx, initialValue)
            stash.unstashAll(
              initialized(initialValue, None, None)
            )
          case (_, m) =>
            stash.stash(m)
            Behaviors.same
        }
      )
    Behaviors.setup[Message[F, S, E]] { ctx =>
      ctx.pipeToSelf(implicitly[FToFuture[F]].apply(init))(_.fold(_ => ???, Messages.Initialize(_)))
      uninitialized
    }
  }

  private case object CancelException extends Exception
}
