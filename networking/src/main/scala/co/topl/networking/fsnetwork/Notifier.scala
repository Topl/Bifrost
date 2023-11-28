package co.topl.networking.fsnetwork

import cats.effect.kernel.{Async, Fiber}
import cats.effect.{Resource, Spawn}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.fsnetwork.Notifier.Message.StartNotifications
import co.topl.networking.fsnetwork.P2PShowInstances._
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
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
    networkConfig:         P2PNetworkConfig,
    slotNotificationFiber: Option[Fiber[F, Throwable, Unit]] = None,
    networkQualityFiber:   Option[Fiber[F, Throwable, Unit]] = None,
    peersUpdateFiber:      Option[Fiber[F, Throwable, Unit]] = None,
    commonAncestorFiber:   Option[Fiber[F, Throwable, Unit]] = None,
    aggressiveP2PFiber:    Option[Fiber[F, Throwable, Unit]] = None
  )

  type Response[F[_]] = State[F]
  type NotifierActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm { case (state, StartNotifications) =>
    startSendingNotifications(state)
  }

  def makeActor[F[_]: Async: Logger](
    peersManager:     PeersManagerActor[F],
    p2pNetworkConfig: P2PNetworkConfig
  ): Resource[F, NotifierActor[F]] = {
    val initialState = State(peersManager, p2pNetworkConfig)
    val actorName = "Notifier actor"
    Actor.makeWithFinalize(actorName, initialState, getFsm, finalizer[F])
  }

  private def startSendingNotifications[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    Logger[F].info(show"Start notifier with config ${state.networkConfig}") >>
    startSlotNotification[F](state)
      .flatMap(startNetworkQualityFiber[F])
      .flatMap(startPeersUpdateFiber[F])
      .flatMap(startCommonAncestorFiber[F])
      .flatMap(startAggressiveP2PFiber[F])
      .map(newState => (newState, newState))

  private def startSlotNotification[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val slotDuration = state.networkConfig.slotDuration

    val updateReputationStream =
      Stream.awakeEvery(slotDuration).evalMap { _ =>
        state.peersManager.sendNoWait(PeersManager.Message.UpdatedReputationTick)
      }

    if (state.slotNotificationFiber.isEmpty && slotDuration.toMillis > 0) {
      for {
        _     <- Logger[F].info(show"Start slot notification fiber")
        fiber <- Spawn[F].start(updateReputationStream.compile.drain)
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

  private def startPeersUpdateFiber[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val peersUpdate = state.networkConfig.peersUpdateInterval

    val peersUpdateStream =
      Stream.awakeEvery(peersUpdate).evalMap { _ =>
        state.peersManager.sendNoWait(PeersManager.Message.UpdatePeersTick)
      }

    if (state.peersUpdateFiber.isEmpty && peersUpdate.toMillis > 0) {
      for {
        _     <- Logger[F].info(show"Start peers update fiber")
        fiber <- Spawn[F].start(peersUpdateStream.compile.drain)
        newState = state.copy(peersUpdateFiber = Option(fiber))
      } yield newState
    } else {
      Logger[F].info(show"Ignoring peers update with interval $peersUpdate") >>
      state.pure[F]
    }
  }

  private def startCommonAncestorFiber[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val commonAncestorInterval = state.networkConfig.networkProperties.commonAncestorTrackInterval

    val commonAncestorTrackStream =
      Stream.awakeEvery(commonAncestorInterval).evalMap { _ =>
        state.peersManager.sendNoWait(PeersManager.Message.PrintP2PState)
      }

    if (state.commonAncestorFiber.isEmpty && commonAncestorInterval.toMillis > 0) {
      for {
        _     <- Logger[F].info(show"Start common ancestor tracking fiber")
        fiber <- Spawn[F].start(commonAncestorTrackStream.compile.drain)
        newState = state.copy(commonAncestorFiber = Option(fiber))
      } yield newState
    } else {
      Logger[F].info(show"Ignoring common ancestor tracking with interval $commonAncestorInterval") >>
      state.pure[F]
    }
  }

  private def startAggressiveP2PFiber[F[_]: Async: Logger](state: State[F]): F[State[F]] = {
    val newHotPeerConnectionInterval = state.networkConfig.aggressiveP2PRequestInterval

    val aggressiveP2PStream =
      Stream.awakeEvery(newHotPeerConnectionInterval).evalMap { _ =>
        state.peersManager.sendNoWait(PeersManager.Message.AggressiveP2PUpdate)
      }

    val aggressiveP2PEnabled = state.networkConfig.networkProperties.aggressiveP2P

    if (state.aggressiveP2PFiber.isEmpty && aggressiveP2PEnabled && newHotPeerConnectionInterval.toMillis > 0) {
      for {
        _     <- Logger[F].info(show"Start aggressive P2P fiber")
        fiber <- Spawn[F].start(aggressiveP2PStream.compile.drain)
        newState = state.copy(aggressiveP2PFiber = Option(fiber))
      } yield newState
    } else {
      Logger[F].info(show"Ignoring aggressive P2P fiber with interval $newHotPeerConnectionInterval") >>
      state.pure[F]
    }
  }

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] =
    Logger[F].info("Stopping notification sending from Notifier actor") >>
    state.slotNotificationFiber.traverse(_.cancel) >>
    state.networkQualityFiber.traverse(_.cancel) >>
    state.peersUpdateFiber.traverse(_.cancel) >>
    state.commonAncestorFiber.traverse(_.cancel) >>
    state.aggressiveP2PFiber.traverse(_.cancel).void

}
