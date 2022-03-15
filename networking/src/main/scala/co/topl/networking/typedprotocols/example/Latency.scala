package co.topl.networking.typedprotocols.example

import cats.data.Chain
import cats.effect.Ref
import cats.implicits._
import cats.{Applicative, Monad}
import co.topl.networking.Parties
import co.topl.networking.typedprotocols.{StateAgency, StateTransition}
import org.typelevel.log4cats.Logger

import java.time.Instant

/**
 * A Typed Protocol which measure the connection latency between two parties.  It is derived from the Ping Pong
 * protocol; however, in the latency protocol, the two parties take turns sending the Ping message.  After sending a
 * Pong message, the sender maintains agency to send a follow-up Ping message.
 */
object Latency {

  object ProtocolStates {
    case object None
    case object ClientIdle
    case object ClientBusy
    case object ServerIdle
    case object ServerBusy
    case object Done
  }

  object ProtocolMessages {
    case object Start
    case object Ping
    case object Pong
    case object Done
  }

  object LocalStates {
    case class Measurements(pingSendTime: Instant, latencyReadings: Chain[Long])
  }

  class StateTransitions[F[_]: Monad: Logger](localPingStateRef: Ref[F, LocalStates.Measurements]) {
    implicit val stateAgentStart: StateAgency[ProtocolStates.None.type] = StateAgency.alwaysA
    implicit val stateAgentClientIdle: StateAgency[ProtocolStates.ClientIdle.type] = StateAgency.alwaysB
    implicit val stateAgentClientBusy: StateAgency[ProtocolStates.ClientBusy.type] = StateAgency.alwaysB
    implicit val stateAgentServerIdle: StateAgency[ProtocolStates.ServerIdle.type] = StateAgency.alwaysA
    implicit val stateAgentServerBusy: StateAgency[ProtocolStates.ServerBusy.type] = StateAgency.alwaysA
    implicit val stateAgentDone: StateAgency[ProtocolStates.Done.type] = StateAgency.noAgent

    implicit val startNoneClientIdle
      : StateTransition[F, ProtocolMessages.Start.type, ProtocolStates.None.type, ProtocolStates.ClientIdle.type] =
      (_, _, _) => ProtocolStates.ClientIdle.pure[F]

    implicit val pingClientIdleServerBusy
      : StateTransition[F, ProtocolMessages.Ping.type, ProtocolStates.ClientIdle.type, ProtocolStates.ServerBusy.type] =
      (_, _, local) =>
        Applicative[F]
          .whenA(local == Parties.B)(localPingStateRef.update(state => state.copy(pingSendTime = Instant.now())))
          .as(ProtocolStates.ServerBusy)

    implicit val pongServerBusyServerIdle
      : StateTransition[F, ProtocolMessages.Pong.type, ProtocolStates.ServerBusy.type, ProtocolStates.ServerIdle.type] =
      (_, _, local) =>
        Applicative[F]
          .whenA(local == Parties.B)(
            localPingStateRef
              .updateAndGet(state =>
                state.copy(latencyReadings =
                  state.latencyReadings.append(System.currentTimeMillis() - state.pingSendTime.toEpochMilli)
                )
              )
              .flatTap(measurements =>
                Logger[F].info(
                  s"Server measured latencies: ${measurements.latencyReadings.map(t => s"${t}ms").mkString_(",")}"
                )
              )
          )
          .as(ProtocolStates.ServerIdle)

    implicit val pingServerIdleClientBusy
      : StateTransition[F, ProtocolMessages.Ping.type, ProtocolStates.ServerIdle.type, ProtocolStates.ClientBusy.type] =
      (_, _, local) =>
        Applicative[F]
          .whenA(local == Parties.A)(localPingStateRef.update(state => state.copy(pingSendTime = Instant.now())))
          .as(ProtocolStates.ClientBusy)

    implicit val pongClientBusyClientIdle
      : StateTransition[F, ProtocolMessages.Pong.type, ProtocolStates.ClientBusy.type, ProtocolStates.ClientIdle.type] =
      (_, _, local) =>
        Applicative[F]
          .whenA(local == Parties.A)(
            localPingStateRef
              .updateAndGet(state =>
                state.copy(latencyReadings =
                  state.latencyReadings.append(System.currentTimeMillis() - state.pingSendTime.toEpochMilli)
                )
              )
              .flatTap(measurements =>
                Logger[F].info(
                  s"Client measured latencies: ${measurements.latencyReadings.map(t => s"${t}ms").mkString_(",")}"
                )
              )
          )
          .as(ProtocolStates.ClientIdle)

    implicit val doneClientIdleDone
      : StateTransition[F, ProtocolMessages.Done.type, ProtocolStates.ClientIdle.type, ProtocolStates.Done.type] =
      (_, _, _) => ProtocolStates.Done.pure[F]

    implicit val doneServerIdleDone
      : StateTransition[F, ProtocolMessages.Done.type, ProtocolStates.ServerIdle.type, ProtocolStates.Done.type] =
      (_, _, _) => ProtocolStates.Done.pure[F]
  }

}
