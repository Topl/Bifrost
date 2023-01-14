package co.topl.actor

import cats.Applicative
import cats.effect.implicits.effectResourceOps
import cats.effect.std.Queue
import cats.effect.{Concurrent, Deferred, IO, Ref, Resource}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import cats.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}

import scala.concurrent.duration.DurationInt

class ActorSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Actor effective handle parallel requests") {
    case class Ping(value: Int)
    case class Pong(response: Long)
    case class State[F[_], I, O](counter: Long)

    def pingFsm[F[_]: Concurrent]: Fsm[F, State[F, Ping, Pong], Ping, Pong] =
      Fsm { case (state, Ping(value)) =>
        (State[F, Ping, Pong](state.counter + value), Pong(state.counter)).pure[F]
      }

    def getActor: F[Actor[F, Ping, Pong]] =
      Actor.make[F, State[F, Ping, Pong], Ping, Pong](State(0), pingFsm[F]).allocated.map(_._1)

    PropF.forAllF { (inputs: Seq[Int]) =>
      for {
        actor: Actor[F, Ping, Pong] <- getActor
        _ <- IO.parTraverseN((inputs.size / 2) + 1)(inputs) { v: Int => actor.sendNoWait(Ping(v)) }
        _ <- actor.send(Ping(0)).map { case Pong(sum) => assertEquals(sum, inputs.map(_.toLong).sum) }
      } yield ()
    }
  }

  test("Actor release all resources taken by child actors") {

    def makeChildActor[F[_]: Concurrent](resourceCounter: Ref[F, Long])(): Resource[F, Actor[F, Unit, Unit]] =
      Actor.makeWithFinalize(
        resourceCounter,
        Fsm[F, Ref[F, Long], Unit, Unit] { case (state, _) =>
          state.get.map(v => println(s"st: $v")) >> state.update(v => v + 1) as (state, ()) },
        (s: Ref[F, Long]) => s.get.map(v => println(s"before remove $v")) >> s.update(s => s - 1)
      )

    case class Ping()
    case class Pong()
    case class State[F[_], I, O](resourceCounter: Ref[F, Long])

    def pingFsm[F[_]: Concurrent](thisActor: Actor[F, Ping, Pong]): Fsm[F, State[F, Ping, Pong], Ping, Pong] =
      Fsm { case (state, Ping()) =>
        state.resourceCounter.get.map(v => println(s"incoming $v")) >>
        //thisActor.acquireActor(makeChildActor(state.resourceCounter)).flatMap(a => a.send(())) >>
        (state, Pong()).pure[F]
      }

    //PropF.forAllF { seed: Int =>
      val count: Long = 0L//seed % 10
      for {
        ref <- Ref.of[F, Long](1)
        _ <- Actor
          .makeFull[F, State[F, Ping, Pong], Ping, Pong](
            State(ref),
            pingFsm[F](_),
            s => s.resourceCounter.get.map(v => println(s"before finish $v")) >>
              s.resourceCounter.update(v => v - 1) >>
              s.resourceCounter.get.map(v => println(s"before finish2 $v"))
          )
          .use { actor =>
            for {
              //_ <- actor.send(Ping())
              _ <- fs2.Stream.range(0, count).evalMap(_ => actor.send(Ping())).compile.drain
              _ <- ref.get.map(assertEquals(_, count + 1))
            } yield ()
          }
        _ <- ref.get.map(assertEquals(_, 0L))
      } yield ()
    }
  //}
}
