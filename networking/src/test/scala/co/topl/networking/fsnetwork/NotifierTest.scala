package co.topl.networking.fsnetwork

import cats.effect.{IO, Sync}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.networking.fsnetwork.NotifierTest.{defaultP2PConfig, F}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object NotifierTest {
  type F[A] = IO[A]
  val defaultSlotDuration: FiniteDuration = FiniteDuration(10, MILLISECONDS)

  val defaultP2PConfig: P2PNetworkConfig =
    P2PNetworkConfig(
      NetworkProperties(
        pingPongInterval = FiniteDuration(20, MILLISECONDS),
        commonAncestorTrackInterval = FiniteDuration(20, MILLISECONDS),
        warmHostsUpdateEveryNBlock = 0.01,
        aggressiveP2P = true
      ),
      defaultSlotDuration
    )
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
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.PrintP2PState)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatedReputationTick)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatePeersTick)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.AggressiveP2PUpdate)
        .atLeastOnce()
        .returns(().pure[F])

      val delay =
        Seq(
          defaultP2PConfig.slotDuration.toMillis,
          defaultP2PConfig.peersUpdateInterval.toMillis,
          defaultP2PConfig.networkProperties.pingPongInterval.toMillis,
          defaultP2PConfig.networkProperties.commonAncestorTrackInterval.toMillis,
          defaultP2PConfig.aggressiveP2PRequestInterval.toMillis
        ).max

      Notifier
        .makeActor(peersManager, defaultP2PConfig)
        .use { actor =>
          for {
            state <- actor.send(Notifier.Message.StartNotifications)
            _ = assert(state.peersUpdateFiber.isDefined)
            _ = assert(state.networkQualityFiber.isDefined)
            _ = assert(state.slotNotificationFiber.isDefined)
            _ = assert(state.commonAncestorFiber.isDefined)
            _ = assert(state.aggressiveP2PFiber.isDefined)
            _ <- Async[F].delayBy(().pure[F], FiniteDuration(delay, MILLISECONDS))
          } yield ()
        }
    }
  }

  test("Notifier shall correctly not started aggressiveP2P if it is disabled ") {
    withMock {
      val peersManager = mock[PeersManagerActor[F]]
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.GetNetworkQualityForWarmHosts)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.PrintP2PState)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatedReputationTick)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatePeersTick)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.AggressiveP2PUpdate)
        .never()
        .returns(().pure[F])

      val delay =
        Seq(
          defaultP2PConfig.slotDuration.toMillis,
          defaultP2PConfig.peersUpdateInterval.toMillis,
          defaultP2PConfig.networkProperties.pingPongInterval.toMillis,
          defaultP2PConfig.networkProperties.commonAncestorTrackInterval.toMillis,
          defaultP2PConfig.aggressiveP2PRequestInterval.toMillis
        ).max

      val netProp = defaultP2PConfig.networkProperties.copy(aggressiveP2P = false)
      Notifier
        .makeActor(peersManager, defaultP2PConfig.copy(networkProperties = netProp))
        .use { actor =>
          for {
            state <- actor.send(Notifier.Message.StartNotifications)
            _ = assert(state.peersUpdateFiber.isDefined)
            _ = assert(state.networkQualityFiber.isDefined)
            _ = assert(state.slotNotificationFiber.isDefined)
            _ = assert(state.commonAncestorFiber.isDefined)
            _ = assert(state.aggressiveP2PFiber.isEmpty)
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
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.PrintP2PState)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatedReputationTick)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.UpdatePeersTick)
        .atLeastOnce()
        .returns(().pure[F])
      (peersManager.sendNoWait _)
        .expects(PeersManager.Message.AggressiveP2PUpdate)
        .atLeastOnce()
        .returns(().pure[F])

      var warmFlag = false
      var networkQualityFlag = false
      var slotNotificationFlag = false
      var commonAncestorFlag = false
      var aggressiveP2PFlag = false

      Notifier
        .makeActor(peersManager, defaultP2PConfig)
        .use { actor =>
          for {
            state <- Sync[F].andWait(
              actor.send(Notifier.Message.StartNotifications),
              FiniteDuration(500, MILLISECONDS)
            )
            _ = state.peersUpdateFiber.get.joinWith({ warmFlag = true }.pure[F])
            _ = state.networkQualityFiber.get.joinWith({ networkQualityFlag = true }.pure[F])
            _ = state.slotNotificationFiber.get.joinWith({ slotNotificationFlag = true }.pure[F])
            _ = state.commonAncestorFiber.get.joinWith({ commonAncestorFlag = true }.pure[F])
            _ = state.aggressiveP2PFiber.get.joinWith({ aggressiveP2PFlag = true }.pure[F])
          } yield ()
        }
        .flatMap { _ =>
          assert(warmFlag).pure[F] >>
          assert(networkQualityFlag).pure[F] >>
          assert(slotNotificationFlag).pure[F] >>
          assert(commonAncestorFlag).pure[F] >>
          assert(aggressiveP2PFlag).pure[F]
        }
    }
  }
}
