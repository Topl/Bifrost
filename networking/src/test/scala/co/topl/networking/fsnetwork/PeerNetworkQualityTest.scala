package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeerNetworkQualityTest.{pingDelay, F}
import co.topl.networking.fsnetwork.ReputationAggregator.Message.PingPongMessagePing
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.networking.p2p.RemoteAddress
import co.topl.node.models.{PingMessage, PongMessage}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object PeerNetworkQualityTest {
  type F[A] = IO[A]
  val pingDelay: FiniteDuration = FiniteDuration(10, MILLISECONDS)
}

class PeerNetworkQualityTest
    extends CatsEffectSuite
    with ScalaCheckEffectSuite
    with AsyncMockFactory
    with TransactionGenerator {
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  val hostId: HostId = RemoteAddress("127.0.0.1", 0)

  test("Ping shall be sent to reputation aggregator") {
    withMock {
      val client = mock[BlockchainPeerClient[F]]
      (client.getPongMessage _).expects(*).once().onCall { ping: PingMessage =>
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
          reputationAggregation
        )
        .use { actor =>
          actor.send(PeerNetworkQuality.Message.GetNetworkQuality)
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
          reputationAggregation
        )
        .use { actor =>
          actor.send(PeerNetworkQuality.Message.GetNetworkQuality) >>
          actor.send(PeerNetworkQuality.Message.GetNetworkQuality) >>
          actor.send(PeerNetworkQuality.Message.GetNetworkQuality)
        }
    }

  }
}
