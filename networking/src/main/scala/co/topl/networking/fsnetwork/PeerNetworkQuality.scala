package co.topl.networking.fsnetwork

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.PeerNetworkQuality.Message.GetNetworkQuality
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.node.models.PingMessage
import org.typelevel.log4cats.Logger

import scala.util.Random

object PeerNetworkQuality {
  sealed trait Message

  object Message {
    case object GetNetworkQuality extends Message
  }

  case class State[F[_]](
    hostId:               HostId,
    client:               BlockchainPeerClient[F],
    reputationAggregator: ReputationAggregatorActor[F]
  )

  type Response[F[_]] = State[F]
  type PeerNetworkQualityActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm { case (state, GetNetworkQuality) =>
    getNetworkQuality(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:               HostId,
    client:               BlockchainPeerClient[F],
    reputationAggregator: ReputationAggregatorActor[F]
  ): Resource[F, Actor[F, Message, Response[F]]] = {
    val initialState = State(hostId, client, reputationAggregator)
    Actor.make(initialState, getFsm[F])
  }

  private def getNetworkQuality[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    getPing(state).value.flatMap { res =>
      val message = ReputationAggregator.Message.PingPongMessagePing(state.hostId, res)
      Logger[F].info(show"From host ${state.hostId} sent quality message: $message") >>
      state.reputationAggregator.sendNoWait(message)
    } >> (state, state).pure[F]

  private val incorrectPongMessage: NetworkQualityError = NetworkQualityError.IncorrectPongMessage: NetworkQualityError
  private val pingMessageSize = 1024 // 1024 hardcoded on protobuf level

  private def getPing[F[_]: Async](state: State[F]): EitherT[F, NetworkQualityError, Long] =
    for {
      pingMessage <- EitherT.liftF(PingMessage(Random.nextString(pingMessageSize)).pure[F])
      before      <- EitherT.liftF(System.currentTimeMillis().pure[F])
      pongMessage <- EitherT.fromOptionF(state.client.getPongMessage(pingMessage), NetworkQualityError.NoPongMessage)
      after       <- EitherT.liftF(System.currentTimeMillis().pure[F])
      pongCorrect = pongMessage.pong.reverse == pingMessage.ping
      ping = after - before
      res <- EitherT.fromEither[F](Either.cond(pongCorrect, ping, incorrectPongMessage))
    } yield res
}
