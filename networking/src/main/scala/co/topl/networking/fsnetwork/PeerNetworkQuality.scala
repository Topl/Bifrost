package co.topl.networking.fsnetwork

import cats.data.EitherT
import cats.effect.kernel.{Async, Fiber}
import cats.effect.{Resource, Spawn}
import cats.implicits._
import co.topl.actor.{Actor, Fsm}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.ReputationAggregator.ReputationAggregatorActor
import co.topl.node.models.PingMessage
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object PeerNetworkQuality {
  sealed trait Message

  object Message {
    case object StartMeasure extends Message

    case object StopMeasure extends Message
  }

  case class State[F[_]](
    hostId:               HostId,
    client:               BlockchainPeerClient[F],
    reputationAggregator: ReputationAggregatorActor[F],
    pingPongInterval:     FiniteDuration,
    measureFiber:         Option[Fiber[F, Throwable, Unit]]
  )

  type Response[F[_]] = State[F]
  type PeerNetworkQualityActor[F[_]] = Actor[F, Message, Response[F]]

  def getFsm[F[_]: Async: Logger]: Fsm[F, State[F], Message, Response[F]] = Fsm {
    case (state, Message.StartMeasure) => startMeasure(state)
    case (state, Message.StopMeasure)  => stopMeasure(state)
  }

  def makeActor[F[_]: Async: Logger](
    hostId:               HostId,
    client:               BlockchainPeerClient[F],
    reputationAggregator: ReputationAggregatorActor[F],
    pingPongInterval:     FiniteDuration
  ): Resource[F, Actor[F, Message, Response[F]]] = {
    val initialState = State(hostId, client, reputationAggregator, pingPongInterval, None)
    Actor.makeWithFinalize(initialState, getFsm[F], finalizer[F])
  }

  private def finalizer[F[_]: Async: Logger](state: State[F]): F[Unit] =
    stopMeasure(state).void

  private def startMeasure[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    if (state.measureFiber.isEmpty) {
      for {
        _     <- Logger[F].info(show"Start network quality fiber for host ${state.hostId}")
        fiber <- Spawn[F].start(startPingPongStream(state).compile.drain)
        newState = state.copy(measureFiber = Option(fiber))
      } yield (newState, newState)
    } else {
      Logger[F].info(show"Ignoring Start network quality fiber for host ${state.hostId}") >>
      (state, state).pure[F]
    }

  private def startPingPongStream[F[_]: Async: Logger](state: State[F]) =
    if (state.pingPongInterval.toMillis > 0) {
      Stream
        .awakeEvery(state.pingPongInterval)
        .evalMap(_ => getPing(state).value)
        .evalMap { res =>
          val message = ReputationAggregator.Message.PingPongMessagePing(state.hostId, res)
          Logger[F].info(show"From host ${state.hostId} sent quality message: $message") >>
          state.reputationAggregator.sendNoWait(message)
        }
    } else {
      Logger[F].error(s"Try to start network quality actor with incorrect ping pong interval ${state.pingPongInterval}")
      Stream.never[F]
    }

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

  private def stopMeasure[F[_]: Async: Logger](state: State[F]): F[(State[F], Response[F])] =
    state.measureFiber
      .map { fiber =>
        val newState = state.copy(measureFiber = None)
        Logger[F].info(s"Stop network quality fiber for host ${state.hostId}") >>
        fiber.cancel >>
        (newState, newState).pure[F]
      }
      .getOrElse {
        Logger[F].info(s"Ignoring stopping network quality fiber for host ${state.hostId}") >>
        (state, state).pure[F]
      }
}
