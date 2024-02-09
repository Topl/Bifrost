package co.topl.networking.typedprotocols.example

import cats.data.Chain
import cats.effect.{Async, IO, Ref}
import cats.implicits._
import co.topl.networking.Parties
import co.topl.networking.typedprotocols.TypedProtocol
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import scala.concurrent.duration._
import scala.util.Random

class LatencySpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  import co.topl.networking.typedprotocols.example.Latency._
  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  // NOTE: This test is disabled because the implementation is just an example.  The implementation necessarily
  // involves "sleeping" which increases the duration of the test run.
  test("play ping pong".ignore) {

    val executorA = TypedProtocol[F](Parties.A)
    val executorB = TypedProtocol[F](Parties.B)
    for {
      localStateRefA <- Ref.of[F, Latency.LocalStates.Measurements](
        Latency.LocalStates.Measurements(Instant.now(), Chain.empty)
      )
      localStateRefB <- Ref.of[F, Latency.LocalStates.Measurements](
        Latency.LocalStates.Measurements(Instant.now(), Chain.empty)
      )
      stateTransitionsA = new StateTransitions[F](localStateRefA)
      stateTransitionsB = new StateTransitions[F](localStateRefB)
      none <- ProtocolStates.None.pure[F]
      idleA <- {
        import stateTransitionsA._
        executorA(ProtocolMessages.Start)(none).nextState
      }
      idleB <- {
        import stateTransitionsB._
        executorB(ProtocolMessages.Start)(none).nextState
      }
      (idleA1, idleB1, _) <- (idleA, idleB, 20)
        .iterateUntilM { case (idleA, idleB, iterationNumber) =>
          (
            {
              import stateTransitionsA._
              executorA(ProtocolMessages.Ping)(idleA).nextState
            }, {
              import stateTransitionsB._
              executorB(ProtocolMessages.Ping)(idleB).nextState
            }
          ).tupled
            .flatTap { case (nextA, nextB) => nextA.pure[F].assertEquals(nextB) }
            .flatMap { case (nextA, nextB) =>
              Async[F].delayBy(
                (
                  {
                    import stateTransitionsA._
                    executorA(ProtocolMessages.Pong)(nextA).nextState
                  }, {
                    import stateTransitionsB._
                    executorB(ProtocolMessages.Pong)(nextB).nextState
                  }
                ).tupled,
                Random.nextInt(200).milli
              )
            }
            .flatTap { case (nextA, nextB) => nextA.pure[F].assertEquals(nextB) }
            .flatMap { case (nextA, nextB) =>
              Async[F].delayBy(
                (
                  {
                    import stateTransitionsA._
                    executorA(ProtocolMessages.Ping)(nextA).nextState
                  }, {
                    import stateTransitionsB._
                    executorB(ProtocolMessages.Ping)(nextB).nextState
                  }
                ).tupled,
                Random.nextInt(200).milli
              )
            }
            .flatTap { case (nextA, nextB) => nextA.pure[F].assertEquals(nextB) }
            .flatMap { case (nextA, nextB) =>
              Async[F].delayBy(
                (
                  {
                    import stateTransitionsA._
                    executorA(ProtocolMessages.Pong)(nextA).nextState
                  }, {
                    import stateTransitionsB._
                    executorB(ProtocolMessages.Pong)(nextB).nextState
                  }
                ).tupled,
                Random.nextInt(200).milli
              )
            }
            .flatTap { case (nextA, nextB) => nextA.pure[F].assertEquals(nextB) }
            .map { case (nextA, nextB) =>
              (nextA, nextB, iterationNumber - 1)
            }
        }(_._3 <= 0)
      done <- {
        import stateTransitionsA._
        executorA(ProtocolMessages.Done)(idleA1).nextState
      }
    } yield done

  }
}
