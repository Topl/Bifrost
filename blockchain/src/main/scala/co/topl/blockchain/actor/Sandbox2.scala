package co.topl.blockchain.actor

import cats.effect.{IO, IOApp, Resource}
import cats.implicits.catsSyntaxApplicativeId
import fs2.Stream

import scala.concurrent.duration.FiniteDuration

object Sandbox2 extends IOApp.Simple {

  var particularActor: Actor[IO, Int, Int] = _
  case class Ping(from: String)
  case class State[IO[_], I, O](counter: Int)

  def pingFsm(actor: Actor[IO, Ping, Int]): Fsm[IO, State[IO, Ping, Int], Ping, Int] =
    Fsm[IO, State[IO, Ping, Int], Ping, Int] { case (state @ State(counter), Ping(who)) =>
      println(s"Hello ${who} from pong to ${actor.id}, current counter is ${counter}")
      Thread.sleep(1000 * (if (counter > 9) 10 else 1))
      val newCounter = counter + 1
      val newState: State[IO, Ping, Int] = state.copy(counter = newCounter)
      println(s"End from ${who} from pong, current counter is ${counter}")

      for {
        createdActor: Actor[IO, Int, Int] <- actor.acquireActor(() =>
          Actor.makeWithFinalize(
            counter,
            Fsm[IO, Int, Int, Int] { case (state: Int, _: Int) => (state, 0).pure[IO] },
            (s: Int) => ().pure[IO].map(_ => println(s"finished child actor $s"))
          )
        )
        _ <- (if (counter == 5) particularActor = createdActor).pure[IO]
        _ <- (if (counter == 6) actor.moveActor(particularActor) else ().pure[IO])
        _ <- (if (counter == 7) particularActor = createdActor).pure[IO]
        _ <- (if (counter == 8) actor.releaseActor(particularActor) else ().pure[IO])
      } yield (newState, newCounter)
    }

  def run: IO[Unit] = {
    val afIO: Resource[IO, Actor[IO, Ping, Int]] =
      Actor.makeFull[IO, State[IO, Ping, Int], Ping, Int](
        State(0),
        pingFsm,
        (s: State[IO, Ping, Int]) => IO(println(s"!!!!!!!!!!!!!!Finished with ${s}"))
      )

    val ioRes: IO[Unit] =
      for {
        (actor: Actor[IO, Ping, Int], finalizer: IO[Unit]) <- afIO.allocated
        stream <- Stream
          .emits(0 to 10)
          .evalMap { v: Int =>
            actor.sendNoWait(Ping(v.toString)) *>
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
