package co.topl.networking.fsnetwork

import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.networking.fsnetwork.NotifierTest.{defaultP2PConfig, F}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object NotifierTest {
  type F[A] = IO[A]
  val defaultSlotDuration: FiniteDuration = FiniteDuration(10, MILLISECONDS)

  val defaultP2PConfig: P2PNetworkConfig =
    P2PNetworkConfig(NetworkProperties(pingPongInterval = FiniteDuration(50, MILLISECONDS)), defaultSlotDuration)
}

class NotifierTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  test("Notifier shall correctly started and send all notifications") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.GetNetworkQualityForWarmHosts)
        .atLeastOnce()
        .returns(().pure[F])

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.ReputationUpdateTick)
        .atLeastOnce()
        .returns(().pure[F])
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.UpdateWarmHosts)
        .atLeastOnce()
        .returns(().pure[F])

      val delay =
        Math.max(
          Math.max(defaultP2PConfig.slotDuration.toMillis, defaultP2PConfig.warmHostsUpdateInterval.toMillis),
          defaultP2PConfig.networkProperties.pingPongInterval.toMillis
        )

      Notifier
        .makeActor(peersManager, reputationAggregator, defaultP2PConfig)
        .use { actor =>
          for {
            state <- actor.send(Notifier.Message.StartNotifications)
            _ = assert(state.warmHostsUpdateFiber.isDefined)
            _ = assert(state.networkQualityFiber.isDefined)
            _ = assert(state.slotNotificationFiber.isDefined)
            _ <- Async[F].delayBy(().pure[F], FiniteDuration(delay, MILLISECONDS))
          } yield ()
        }
    }
  }

  test("Notifier shall correctly shutdown all fibers") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.GetNetworkQualityForWarmHosts)
        .anyNumberOfTimes()
        .returns(().pure[F])

      val reputationAggregator = mock[ReputationAggregatorActor[F]]
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.ReputationUpdateTick)
        .anyNumberOfTimes()
        .returns(().pure[F])
      (reputationAggregator.sendNoWait _)
        .expects(ReputationAggregator.Message.UpdateWarmHosts)
        .anyNumberOfTimes()
        .returns(().pure[F])

      var warmFlag = false
      var networkQualityFlag = false
      var slotNotificationFlag = false
      Notifier
        .makeActor(peersManager, reputationAggregator, defaultP2PConfig)
        .use { actor =>
          for {
            state <- actor.send(Notifier.Message.StartNotifications)
            _ = state.warmHostsUpdateFiber.get.joinWith({ warmFlag = true }.pure[F])
            _ = state.networkQualityFiber.get.joinWith({ networkQualityFlag = true }.pure[F])
            _ = state.slotNotificationFiber.get.joinWith({ slotNotificationFlag = true }.pure[F])
          } yield ()
        }
        .flatMap { _ =>
          assert(warmFlag).pure[F] >>
          assert(networkQualityFlag).pure[F] >>
          assert(slotNotificationFlag).pure[F]
        }
    }
  }
}
