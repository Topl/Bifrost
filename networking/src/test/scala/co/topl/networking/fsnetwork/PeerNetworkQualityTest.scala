package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeerNetworkQualityTest.F
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
      val pingPongInterval = FiniteDuration(100, MILLISECONDS)
      val pingDelay = 10

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).atLeastOnce().onCall { ping: PingMessage =>
        Thread.sleep(pingDelay)
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).atLeastOnce().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay)
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
            _ <- actor.send(PeerNetworkQuality.Message.StartMeasure)
            _ = Thread.sleep(pingPongInterval.toMillis + pingDelay * 5)
          } yield ()
        }
    }

  }

  test("Ping shall be started: one success and two errors") {
    withMock {
      val pingPongInterval = FiniteDuration(100, MILLISECONDS)
      val pingDelay = 10

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).once().onCall { ping: PingMessage =>
        Thread.sleep(pingDelay)
        Option(PongMessage(ping.ping.reverse)).pure[F]
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
            assert(t >= pingDelay)
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
          for {
            _ <- actor.send(PeerNetworkQuality.Message.StartMeasure)
            _ = Thread.sleep(3 * pingPongInterval.toMillis + pingDelay * 5)
          } yield ()
        }
    }

  }

  test("Ping shall not be started if interval is equal to 0") {
    withMock {
      val pingPongInterval = FiniteDuration(100, MILLISECONDS)
      val pingDelay = 0

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).never().onCall { ping: PingMessage =>
        Thread.sleep(pingDelay)
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).never().onCall { message: ReputationAggregator.Message =>
        message match {
          case PingPongMessagePing(`hostId`, Right(t)) =>
            assert(t >= pingDelay)
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
            _ <- actor.send(PeerNetworkQuality.Message.StartMeasure)
            _ = Thread.sleep(pingPongInterval.toMillis + pingDelay * 5)
          } yield ()
        }
    }

  }

  test("Start and stop measuring shall works correctly") {
    withMock {
      val pingPongInterval = FiniteDuration(100, MILLISECONDS)
      val pingDelay = 10

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).anyNumberOfTimes().onCall { ping: PingMessage =>
        Thread.sleep(pingDelay)
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).anyNumberOfTimes().onCall {
        message: ReputationAggregator.Message =>
          message match {
            case PingPongMessagePing(`hostId`, Right(t)) =>
              assert(t >= pingDelay)
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
      val pingPongInterval = FiniteDuration(100, MILLISECONDS)
      val pingDelay = 10

      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).anyNumberOfTimes().onCall { ping: PingMessage =>
        Thread.sleep(pingDelay)
        Option(PongMessage(ping.ping.reverse)).pure[F]
      }

      val reputationAggregation = mock[ReputationAggregatorActor[F]]
      (reputationAggregation.sendNoWait _).expects(*).anyNumberOfTimes().onCall {
        message: ReputationAggregator.Message =>
          message match {
            case PingPongMessagePing(`hostId`, Right(t)) =>
              assert(t >= pingDelay)
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
