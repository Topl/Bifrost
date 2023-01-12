package co.topl.blockchain.network

import akka.actor.FSM
import cats.effect.implicits.{asyncOps, genSpawnOps}
import cats.effect.unsafe.implicits.global
import cats.effect.{Concurrent, Deferred, IO, IOApp, Resource, Temporal}
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxApply, toFlatMapOps, toFunctorOps}
import cats.effect._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import cats.syntax.parallel._
import fs2.concurrent.SignallingRef
import fs2.{concurrent, Stream}

import java.lang.Runtime.getRuntime

object Sandbox2 extends IOApp.Simple {
  type F[A] = IO[A]
  case class Ping2(from: String)

  case class State[F[_], I, O](counter: Int)

  var particularActor: Actor[IO, Int, Int] = _

  def pongBehaviour2(actor: Actor[IO, Ping2, Int]): Fsm[IO, State[IO, Ping2, Int], Ping2, Int] = Fsm[IO, State[IO, Ping2, Int], Ping2, Int] {
    case (state@State(counter), Ping2(who)) =>
      println(s"Hello ${who} from pong to ${actor.id}, current counter is ${counter}")
      Thread.sleep(1000 * (if (counter > 9) 10 else 1))
      val newCounter = counter + 1
      val newState: State[IO, Ping2, Int] = state.copy(counter = newCounter)
      println(s"End from ${who} from pong, current counter is ${counter}")

      for {
        createdActor: Actor[IO, Int, Int] <- actor.acquireActor(() => Actor.makeWithFinalize(
          counter,
          Fsm[IO, Int, Int, Int] { case (state: Int, _: Int) => (state, 0).pure[IO] },
          (s: Int) => ().pure[IO].map(_ => println(s"finished child actor $s"))))
        _ <- (if (counter == 5) particularActor = createdActor).pure[IO]
        _ <- (if (counter == 6) actor.moveActor(particularActor) else ().pure[IO])
        _ <- (if (counter == 7) particularActor = createdActor).pure[IO]
        _ <- (if (counter == 8) actor.removeActor(particularActor) else ().pure[IO])
      } yield (newState, newCounter)
  }

  def run: IO[Unit] = {
    val afIO: Resource[IO, Actor[IO, Ping2, Int]] =
      Actor.makeFull[IO, State[IO, Ping2, Int], Ping2, Int](State(0), pongBehaviour2, (s: State[IO, Ping2, Int]) => IO(println(s"!!!!!!!!!!!!!!Finished with ${s}")))
    // var (actor, finalizer) = afIO.allocated//.unsafeRunSync()

    val ioRes: IO[Unit] =
      for {
        (actor: Actor[IO, Ping2, Int], finalizer: IO[Unit]) <- afIO.allocated
        stream <- Stream
          .emits(0 to 10)
          .evalMap { v: Int =>
            actor.sendNoWait(Ping2(v.toString)) *>
            //actor.mailboxSize.map(s => println(s"mailboxsize: $s for actor ${actor.id}")) *>
            IO().flatMap { _ =>
              if (v == 5) {
                actor.gracefulShutdown(finalizer) *>
                ().pure[IO].map(_ => println("started shutdown"))
              } else ().pure[IO]
            } *>
            IO(())
          }
          .compile
          .drain
      } yield stream

    ioRes *> IO.sleep(FiniteDuration(30, "s"))

  }
}
