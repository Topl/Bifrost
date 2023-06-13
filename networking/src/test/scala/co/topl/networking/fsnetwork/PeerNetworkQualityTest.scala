package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeerNetworkQualityTest.{pingDelay, pingPongInterval, F}
import co.topl.networking.fsnetwork.ReputationAggregator.Message.PingPongMessagePing
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.node.models.{PingMessage, PongMessage}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerNetworkQualityTest {
  type F[A] = IO[A]

  val pingPongInterval: FiniteDuration = FiniteDuration(100, MILLISECONDS)
  val pingDelay: FiniteDuration = FiniteDuration(10, MILLISECONDS)
}

class PeerNetworkQualityTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = "127.0.0.1"

  test("Ping shall be started and result shall be sent to reputation aggregator") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).atLeastOnce().onCall { ping: PingMessage =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).atLeastOnce().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay.toMillis)
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }

      PeerNetworkQuality
        .makeActor(
          hostId,
          client,
          reputationAggregation,
          pingPongInterval
        )
        .use { actor =>
          Async[F].andWait(actor.send(PeerNetworkQuality.Message.StartMeasure), pingPongInterval + pingDelay * 5)
        }
    }

  }

  test("Ping shall be started: one success and two errors") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).once().onCall { ping: PingMessage =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }
      (client.getPongMessage _).expects(*).once().onCall { _: PingMessage =>
        Option.empty[PongMessage].pure[F]
      }
      (client.getPongMessage _).expects(*).atLeastOnce().onCall { ping: PingMessage =>
        Option(PongMessage(ping.ping)).pure[F]
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).once().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay.toMillis)
            ().pure[F]
          case _ => throw new IllegalStateException()
        }
      }
      (reputationAggregation.sendNoWait _).expects(*).once().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Left(NoPongMessage)) => ().pure[F]
          case _                                                  => throw new IllegalStateException()
        }
      }
      (reputationAggregation.sendNoWait _).expects(*).atLeastOnce().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Left(IncorrectPongMessage)) => ().pure[F]
          case _                                                         => throw new IllegalStateException()
        }
      }

      PeerNetworkQuality
        .makeActor(
          hostId,
          client,
          reputationAggregation,
          pingPongInterval
        )
        .use { actor =>
          Async[F].andWait(actor.send(PeerNetworkQuality.Message.StartMeasure), pingPongInterval * 3 + pingDelay * 5)
        }
    }

  }

  test("Ping shall not be started if interval is equal to 0") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      val reputationAggregation = mock[ReputationAggregatorActor[F]]

      PeerNetworkQuality
        .makeActor(
          hostId,
          client,
          reputationAggregation,
          FiniteDuration(0, MILLISECONDS)
        )
        .use { actor =>
          Async[F]
            .andWait(actor.send(PeerNetworkQuality.Message.StartMeasure), pingPongInterval + pingDelay * 5)
            .flatMap(state => assert(state.measureFiber.isEmpty).pure[F])
        }
    }

  }

  test("Start and stop measuring shall works correctly") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).anyNumberOfTimes().onCall { ping: PingMessage =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).anyNumberOfTimes().onCall {
        message: ReputationAggregator.Message =>
          message match {
            case PingPongMessagePing(`hostId`, Right(t)) =>
              assert(t >= pingDelay.toMillis)
              ().pure[F]
            case _ => throw new IllegalStateException()
          }
      }

      PeerNetworkQuality
        .makeActor(
          hostId,
          client,
          reputationAggregation,
          pingPongInterval
        )
        .use { actor =>
          for {
            stateAfterStart <- actor.send(PeerNetworkQuality.Message.StartMeasure)
            _ = assert(stateAfterStart.measureFiber.isDefined)
            stateAfterStop <- actor.send(PeerNetworkQuality.Message.StopMeasure)
            _ = assert(stateAfterStop.measureFiber.isEmpty)
          } yield ()
        }
    }
  }

  test("Fiber is shutdown after release of actor") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).anyNumberOfTimes().onCall { ping: PingMessage =>
        Async[F].delayBy(Option(PongMessage(ping.ping.reverse)).pure[F], pingDelay)
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).anyNumberOfTimes().onCall {
        message: ReputationAggregator.Message =>
          message match {
            case PingPongMessagePing(`hostId`, Right(t)) =>
              assert(t >= pingDelay.toMillis)
              ().pure[F]
            case _ => throw new IllegalStateException()
          }
      }

      var flag = false
      PeerNetworkQuality
        .makeActor(
          hostId,
          client,
          reputationAggregation,
          pingPongInterval
        )
        .use { actor =>
          actor.send(PeerNetworkQuality.Message.StartMeasure).map { state =>
            state.measureFiber.get.joinWith({ flag = true }.pure[F])
          }
        }
        .flatMap(_ => assert(flag).pure[F])
    }
  }

}
