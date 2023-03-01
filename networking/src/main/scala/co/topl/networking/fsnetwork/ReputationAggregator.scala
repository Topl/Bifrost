package co.topl.networking.fsnetwork

import cats.Applicative
import cats.effect.{Concurrent, Resource}
import cats.implicits.catsSyntaxApplicativeId
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.fsnetwork.PeersManager.PeersManagerActor
import co.topl.networking.fsnetwork.ReputationAggregator.Message.UpdatePeerReputation

//TODO Reputation mechanism is not defined yet
object ReputationAggregator {
  case class State[F[_]](peerManager: PeersManagerActor[F], reputation: Map[HostId, HostReputationValue])
  type Response[F[_]] = State[F]

  sealed trait Message

  object Message {
    case class UpdatePeerReputation(hostId: HostId, hostReputationValue: HostReputationValue) extends Message
  }

  type ReputationAggregatorActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] =
    Fsm { case (currentState, update: UpdatePeerReputation) =>
      updatePeerReputation(currentState, update)
    }

  private def updatePeerReputation[F[_]: Applicative](
    currentState:     State[F],
    reputationUpdate: UpdatePeerReputation
  ) =
    (currentState, currentState).pure[F]

  def makeActor[F[_]: Concurrent](peersManager: PeersManagerActor[F]): Resource[F, ReputationAggregatorActor[F]] = {
    val initialState = ReputationAggregator.State[F](peersManager, Map.empty[HostId, HostReputationValue])
    Actor.make[F, State[F], Message, Response[F]](initialState, getFsm)
  }
}
