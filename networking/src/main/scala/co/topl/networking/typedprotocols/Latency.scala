package co.topl.networking.typedprotocols

import cats.Monad
import cats.data.Chain
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}
import org.typelevel.log4cats.Logger

import java.time.Instant

/**
 * A Typed Protocol which measure the connection latency between two parties.  It is derived from the Ping Pong
 * protocol; however, in the latency protocol, the two parties take turns sending the Ping message.  After sending a
 * Pong message, the sender maintains agency to send a follow-up Ping message.
 */
object Latency {

  object States {
    case class None()
    case class Idle()
    case class Busy()
    case class Done()
  }

  object Messages {
    case class Start()
    case class Ping()
    case class Pong()
    case class Done()
  }

  case class LocalPingState(pingSendTime: Instant, latencyReadings: Chain[Long])

  class StateTransitions[F[_]: Sync: Logger](localPingStateRef: Ref[F, LocalPingState]) {

    implicit val startNoneIdle: StateTransition[F, Messages.Start, States.None, States.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, States.Idle()).pure[F]

    implicit val pingIdleBusy: StateTransition[F, Messages.Ping, States.Idle, States.Busy] =
      (_, protocolInState, local) =>
        (
          TypedProtocolState(protocolInState.currentAgent.map(_.opposite), States.Busy()).pure[F],
          Monad[F].whenA(protocolInState.currentAgent.contains(local))(
            localPingStateRef.update(state => state.copy(pingSendTime = Instant.now()))
          )
        ).mapN((next, _) => next)

    implicit val pongBusyIdle: StateTransition[F, Messages.Pong, States.Busy, States.Idle] =
      (_, protocolInState, local) =>
        (
          TypedProtocolState(protocolInState.currentAgent, States.Idle()).pure[F],
          Monad[F].whenA(!protocolInState.currentAgent.contains(local))(
            localPingStateRef
              .updateAndGet(state =>
                state.copy(latencyReadings =
                  state.latencyReadings.append(System.currentTimeMillis() - state.pingSendTime.toEpochMilli)
                )
              )
              .flatTap(state =>
                Logger[F].info(
                  s"Measured latencies=${state.latencyReadings.map(l => s"${l}ms").toList.mkString(",")} localParty=$local"
                )
              )
              .void
          )
        ).mapN((next, _) => next)

    implicit val doneIdleDone: StateTransition[F, Messages.Done, States.Idle, States.Done] =
      (_, _, _) => TypedProtocolState(none, States.Done()).pure[F]
  }

}
