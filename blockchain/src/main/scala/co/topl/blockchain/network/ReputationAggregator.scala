package co.topl.blockchain.network

import cats.Applicative
import cats.effect.{Concurrent, Resource}
import cats.implicits.catsSyntaxApplicativeId
import co.topl.blockchain.network.PeersManager.PeersManagerActor
import co.topl.blockchain.network.PeerState.PeerState
import co.topl.blockchain.network.ReputationAggregator.Message.{SetPeerManager, UpdatePeerReputation}

import scala.collection.immutable.TreeMap

object ReputationAggregator {
  case class State[F[_]](peerManager: Option[PeersManagerActor[F]], reputation: Map[HostId, HostReputationValue])
  type Response[F[_]] = State[F]

  sealed trait Message

  object Message {
    case class UpdatePeerReputation(hostId: HostId, hostReputationValue: HostReputationValue) extends Message
    case class SetPeerManager[F[_]](peerManager: PeersManagerActor[F]) extends Message
  }

  type ReputationAggregatorActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Concurrent]: Fsm[F, State[F], Message, Response[F]] =
    Fsm {
      case (currentState, update: UpdatePeerReputation)     => updatePeerReputation(currentState, update)
      case (currentState, newAggregator: SetPeerManager[F]) => setPeerManager(currentState, newAggregator)
    }

  private val reputationValueToReputation =
    TreeMap[HostReputationValue, PeerState](
      0L -> PeerState.Banned,
      20L -> PeerState.Cold,
      40L -> PeerState.Warm,
      60L -> PeerState.Hot
    )

  private def getReputation(hostReputationValue: HostReputationValue): PeerState =
    reputationValueToReputation.minAfter(hostReputationValue).map(_._2).getOrElse(PeerState.Hot)

  private def updatePeerReputation[F[_]: Applicative](
    currentState:     State[F],
    reputationUpdate: UpdatePeerReputation
  ) = {
    val hostId = reputationUpdate.hostId
    val reputationDelta = reputationUpdate.hostReputationValue

    val (newStateOpt, reputationValue) =
      currentState.reputation.get(hostId) match {
        case Some(previousReputationValue) =>
          val previousReputation = getReputation(previousReputationValue)
          val newReputationValue = previousReputationValue + reputationDelta
          val newReputation = getReputation(newReputationValue)
          (Option.when(newReputation != previousReputation)(newReputation), newReputationValue)
        case None => (Option(getReputation(reputationDelta)), reputationDelta)
      }

    for {
      newState    <- newStateOpt
      peerManager <- currentState.peerManager
    } yield (peerManager.sendNoWait(PeersManager.Message.UpdatePeerStatus(hostId, newState)))

    val newReputationMap = currentState.reputation + (hostId -> reputationValue)
    val newState = currentState.copy(reputation = newReputationMap)

    (newState, newState).pure[F]
  }

  private def setPeerManager[F[_]: Applicative](
    currentState:          State[F],
    setPeerManagerMessage: SetPeerManager[F]
  ) = {
    val newState = currentState.copy(peerManager = Option(setPeerManagerMessage.peerManager))
    (newState, newState).pure[F]
  }

  def makeActor[F[_]: Concurrent]: Resource[F, ReputationAggregatorActor[F]] = {
    val initialState = ReputationAggregator.State[F](None, Map.empty[HostId, HostReputationValue])
    Actor.make[F, State[F], Message, Response[F]](initialState, getFsm)
  }
}
