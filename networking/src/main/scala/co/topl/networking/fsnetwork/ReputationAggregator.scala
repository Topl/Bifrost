package co.topl.networking.fsnetwork

import cats.Applicative
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.Message.{PingPongMessagePing, UpdatePeerReputation}
import org.typelevel.log4cats.Logger

//TODO Reputation mechanism is not defined yet
object ReputationAggregator {
  case class State[F[_]](peerManager: PeersManagerActor[F], reputation: Map[HostId, HostReputationValue])
  type Response[F[_]] = State[F]

  sealed trait Message

  object Message {
    case class UpdatePeerReputation(hostId: HostId, hostReputationValue: HostReputationValue) extends Message
    case class PingPongMessagePing(hostId: HostId, delay: Either[NetworkQualityError, Long]) extends Message
  }

  type ReputationAggregatorActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (currentState, update: UpdatePeerReputation)     => updatePeerReputation(currentState, update)
      case (currentState, pongMessage: PingPongMessagePing) => pongMessageProcessing(currentState, pongMessage)
    }

  private def updatePeerReputation[F[_]: Applicative](
    currentState:     State[F],
    reputationUpdate: UpdatePeerReputation
  ) =
    (currentState, currentState).pure[F]

  private def pongMessageProcessing[F[_]: Async: Logger](
    currentState:     State[F],
    reputationUpdate: PingPongMessagePing
  ) =
    Logger[F].info(show"Received pong message from host ${reputationUpdate.hostId}") >>
    (currentState, currentState).pure[F]

  def makeActor[F[_]: Async: Logger](peersManager: PeersManagerActor[F]): Resource[F, ReputationAggregatorActor[F]] = {
    val initialState = ReputationAggregator.State[F](peersManager, Map.empty[HostId, HostReputationValue])
    Actor.make[F, State[F], Message, Response[F]](initialState, getFsm)
  }
}
