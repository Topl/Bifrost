package co.topl.blockchain.network

import akka.actor.FSM
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, IOApp}

import scala.concurrent.duration.DurationInt
import cats.syntax.parallel._

object Sandbox2 extends IOApp.Simple{
  type F[A] = IO[A]
  case class Ping2(from: String)

  val pongBehaviour2: Fsm[IO, Int, Ping2, Int] = Fsm.pure[IO, Int, Ping2, Int] {
    case (currentState: Int, Ping2(who)) =>
      println(s"Hello ${who} from pong, current counter is ${currentState}")
      Thread.sleep(100)
      val newState = currentState + 1
      println(s"End from ${who} from pong, current counter is ${currentState}")
      (newState, newState)
  }

  override def run: IO[Unit] = {
    Actor.make(0, pongBehaviour2).use {pong: Actor[IO, Ping2, Int] =>
      IO.println("start") *>
      (
        pong
        .send(Ping2("Sytherax"))
        .timeout(2.second)
        .flatMap { c => IO.println(s"Ping Response: ${c}") },
        pong.send(Ping2("Asterix"))
          .timeout(3.second)
          .flatMap { c => IO.println(s"Ping Response: ${c}") },
        pong.sendNoWait(Ping2("NoWait"))
          .timeout(2.second)
          .flatMap { c => IO.println(s"Ping Response: ${c}") },
      ).parMapN((_, _, _) => ())
    }.void
  }
}
