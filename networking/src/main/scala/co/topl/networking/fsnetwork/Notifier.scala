package co.topl.networking.fsnetwork

import cats.effect.kernel.Fiber
import cats.effect.kernel.Async
import cats.effect.{Resource, Spawn}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.fsnetwork.Notifier.Message.StartNotifications
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import fs2.Stream
import org.typelevel.log4cats.Logger

object Notifier {
  sealed trait Message

  object Message {

    /**
     * Request current tip from remote peer
     */
    case object StartNotifications extends Message
  }

  case class State[F[_]](
    peersManager:          PeersManagerActor[F],
    reputationAggregator:  ReputationAggregatorActor[F],
    networkConfig:         P2PNetworkConfig,
    slotNotificationFiber: Option[Fiber[F, Throwable, Unit]] = None,
    networkQualityFiber:   Option[Fiber[F, Throwable, Unit]] = None,
    warmHostsUpdateFiber:  Option[Fiber[F, Throwable, Unit]] = None
  )

  type Response[F[_]] = State[F]
  type NotifierActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm { case (state, StartNotifications) =>
    startSendingNotifications(state)
  }

  def makeActor[F[_]: Async: Logger](
    peersManager:         PeersManagerActor[F],
    reputationAggregator: ReputationAggregatorActor[F],
    p2pNetworkConfig:     P2PNetworkConfig
  ): Resource[F, NotifierActor[F]] = {
    val initialState = State(peersManager, reputationAggregator, p2pNetworkConfig)
    Actor.makeWithFinalize(initialState, getFsm, finalizer[F])
  }

  private def startSendingNotifications[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(s"Start notifier with config ${state.networkConfig}") >>
    startSlotNotification[F](state)
      .flatMap(startNetworkQualityFiber[F])
      .flatMap(startWarmHostsUpdateFiber[F])
      .map(newState => (newState, newState))

  private def startSlotNotification[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val slotDuration = state.networkConfig.slotDuration

    val sendPingStream =
      Stream.awakeEvery(slotDuration).evalMap { _ =>
        state.reputationAggregator.sendNoWait(ReputationAggregator.Message.ReputationUpdateTick)
      }

    if (state.slotNotificationFiber.isEmpty && slotDuration.toMillis > 0) {
      for {
        _     <- Logger[F].info(show"Start slot notification fiber")
        fiber <- Spawn[F].start(sendPingStream.compile.drain)
        newState = state.copy(slotNotificationFiber = Option(fiber))
      } yield newState
    } else {
      Logger[F].info(show"Ignoring starting slot notification fiber with interval $slotDuration") >>
      state.pure[F]
    }
  }

  private def startNetworkQualityFiber[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val pingPongInterval = state.networkConfig.networkProperties.pingPongInterval

    val networkQualityStream =
      Stream.awakeEvery(pingPongInterval).evalMap { _ =>
        state.peersManager.sendNoWait(PeersManager.Message.GetNetworkQualityForWarmHosts)
      }

    if (state.networkQualityFiber.isEmpty && pingPongInterval.toMillis > 0) {
      for {
        _     <- Logger[F].info(show"Start network quality notifier fiber")
        fiber <- Spawn[F].start(networkQualityStream.compile.drain)
        newState = state.copy(networkQualityFiber = Option(fiber))
      } yield newState
    } else {
      Logger[F].info(show"Ignoring starting network quality notifier fiber with interval $pingPongInterval") >>
      state.pure[F]
    }
  }

  private def startWarmHostsUpdateFiber[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val warmPeersUpdate = state.networkConfig.warmHostsUpdateInterval

    val warmHostsUpdateStream =
      Stream.awakeEvery(warmPeersUpdate).evalMap { _ =>
        state.reputationAggregator.sendNoWait(ReputationAggregator.Message.UpdateWarmHosts)
      }

    if (state.warmHostsUpdateFiber.isEmpty && warmPeersUpdate.toMillis > 0) {
      for {
        _     <- Logger[F].info(show"Start warm hosts update fiber")
        fiber <- Spawn[F].start(warmHostsUpdateStream.compile.drain)
        newState = state.copy(warmHostsUpdateFiber = Option(fiber))
      } yield newState
    } else {
      Logger[F].info(show"Ignoring warm hosts update with interval $warmPeersUpdate") >>
      state.pure[F]
    }
  }

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] =
    Logger[F].info("Stopping notification sending from Notifier actor") >>
    state.slotNotificationFiber.map(_.cancel).getOrElse(().pure[F]) >>
    state.networkQualityFiber.map(_.cancel).getOrElse(().pure[F]) >>
    state.warmHostsUpdateFiber.map(_.cancel).getOrElse(().pure[F])

}
