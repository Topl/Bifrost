package co.topl.networking.fsnetwork

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.Message._
import org.typelevel.log4cats.Logger

//TODO Reputation mechanism is not fully defined yet
object ReputationAggregator {

  case class State[F[_]](
    peerManager:              PeersManagerActor[F],
    performanceReputation:    Map[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue],
    newnessReputation:        Map[HostId, HostReputationValue]
  )

  type Response[F[_]] = State[F]

  sealed trait Message

  object Message {
    case class RemoveReputationForHost(hostId: HostId) extends Message

    case class PingPongMessagePing(hostId: HostId, response: Either[NetworkQualityError, Long]) extends Message
    case class DownloadTimeHeader(hostId: HostId, delay: Long) extends Message
    case class DownloadTimeBody(hostId: HostId, bodyDelay: Long, txDelays: Seq[Long]) extends Message
    case class HostProvideIncorrectBlock(hostId: HostId) extends Message
  }

  type ReputationAggregatorActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (state, RemoveReputationForHost(hostId))   => removeReputationForHost(state, hostId)
      case (state, HostProvideIncorrectBlock(hostId)) => incorrectBlockReceived(state, hostId)

      case (state, PingPongMessagePing(hostId, pongResponse)) => pongMessageProcessing(state, hostId, pongResponse)
      case (state, DownloadTimeHeader(hostId, delay))         => headerDownloadTime(state, hostId, delay)
      case (state, DownloadTimeBody(hostId, delay, txDelays)) => blockDownloadTime(state, hostId, delay, txDelays)
    }

  def makeActor[F[_]: Async: Logger](
    peersManager:             PeersManagerActor[F],
    performanceReputation:    Map[HostId, HostReputationValue] = Map.empty[HostId, HostReputationValue],
    blockProvidingReputation: Map[HostId, HostReputationValue] = Map.empty[HostId, HostReputationValue],
    newnessReputation:        Map[HostId, HostReputationValue] = Map.empty[HostId, HostReputationValue]
  ): Resource[F, ReputationAggregatorActor[F]] = {
    val initialState = ReputationAggregator.State[F](
      peersManager,
      performanceReputation,
      blockProvidingReputation,
      newnessReputation
    )
    Actor.make[F, State[F], Message, Response[F]](initialState, getFsm)
  }

  private def removeReputationForHost[F[_]: Async: Logger](
    state:  State[F],
    hostId: HostId
  ): F[(State[F], Response[F])] = {
    val newState = state.copy(
      performanceReputation = state.performanceReputation - hostId,
      blockProvidingReputation = state.blockProvidingReputation - hostId,
      newnessReputation = state.newnessReputation - hostId
    )

    Logger[F].info(show"Remove reputation for host $hostId") >>
    (newState, newState).pure[F]
  }

  private def pongMessageProcessing[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    response: Either[NetworkQualityError, Long]
  ): F[(State[F], Response[F])] =
    response match {
      case Right(value) =>
        val newReputation = delayToReputation(value)
        val newPerformanceReputation =
          state.performanceReputation + (hostId -> newReputation)
        val newState = state.copy(performanceReputation = newPerformanceReputation)
        Logger[F].info(show"Got pong message delay $value from remote host $hostId") >>
        (newState, newState).pure[F]

      case Left(error) =>
        Logger[F].error(show"Bad pong message: $error from host $hostId") >>
        state.peerManager.sendNoWait(PeersManager.Message.UpdatePeerStatus(hostId, PeerState.Banned)) >>
        (state, state).pure[F]
    }

  private def incorrectBlockReceived[F[_]: Async: Logger](
    state:  State[F],
    hostId: HostId
  ): F[(State[F], Response[F])] =
    Logger[F].error(show"Received incorrect block from host $hostId") >>
    state.peerManager.sendNoWait(PeersManager.Message.UpdatePeerStatus(hostId, PeerState.Banned)) >>
    (state, state).pure[F]

  private def headerDownloadTime[F[_]: Async: Logger](
    state:  State[F],
    hostId: HostId,
    delay:  Long
  ): F[(State[F], Response[F])] = {
    val newReputation = delayToReputation(delay)
    val updatedReputation = state.performanceReputation
      .get(hostId)
      .map { currentReputation =>
        (currentReputation * 2 + newReputation) / 3.0
      }
      .getOrElse(newReputation)

    val newPerformanceReputation: Map[HostId, HostReputationValue] =
      state.performanceReputation + (hostId -> updatedReputation)
    val newState = state.copy(performanceReputation = newPerformanceReputation)

    Logger[F].debug(show"Received header download from host $hostId with delay $delay") >>
    (newState, newState).pure[F]
  }

  private def blockDownloadTime[F[_]: Async: Logger](
    state:    State[F],
    hostId:   HostId,
    delay:    Long,
    tsDelays: Seq[Long]
  ): F[(State[F], Response[F])] = {
    val maxDelay = (tsDelays :+ delay).max
    val newReputation = delayToReputation(maxDelay)
    val updatedReputation = state.performanceReputation
      .get(hostId)
      .map { currentReputation =>
        (currentReputation * 2 + newReputation) / 3.0
      }
      .getOrElse(newReputation)

    val newPerformanceReputation: Map[HostId, HostReputationValue] =
      state.performanceReputation + (hostId -> updatedReputation)
    val newState = state.copy(performanceReputation = newPerformanceReputation)

    Logger[F].debug(show"Received block download from host $hostId with max delay $delay") >>
    (newState, newState).pure[F]
  }

  def delayToReputation(delayInMs: Long): Double = {
    val zeroReputationDelay = 2000.0
    val reputationReducing = delayInMs.toDouble / zeroReputationDelay

    1.0 - reputationReducing
  }
}
