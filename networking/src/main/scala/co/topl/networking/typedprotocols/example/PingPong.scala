package co.topl.networking.typedprotocols.example

import cats.Applicative
import cats.implicits._
import co.topl.networking.typedprotocols.{StateAgency, StateTransition}

object PingPong {

  object ProtocolStates {
    case object None
    case object Idle
    case object Busy
    case object Done
  }

  object ProtocolMessages {
    case object Start
    case object Ping
    case object Pong
    case object Done
  }

  object StateTransitions {
    implicit val stateAgentStart: StateAgency[ProtocolStates.None.type] = StateAgency.alwaysA
    implicit val stateAgentIdle: StateAgency[ProtocolStates.Idle.type] = StateAgency.alwaysB
    implicit val stateAgentBusy: StateAgency[ProtocolStates.Busy.type] = StateAgency.alwaysB
    implicit val stateAgentDone: StateAgency[ProtocolStates.Done.type] = StateAgency.noAgent

    implicit def startNoneIdle[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Start.type, ProtocolStates.None.type, ProtocolStates.Idle.type] =
      (_, _, _) => ProtocolStates.Idle.pure[F]

    implicit def pingIdleBusy[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Ping.type, ProtocolStates.Idle.type, ProtocolStates.Busy.type] =
      (_, _, _) => ProtocolStates.Busy.pure[F]

    implicit def pongBusyIdle[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Pong.type, ProtocolStates.Busy.type, ProtocolStates.Idle.type] =
      (_, _, _) => ProtocolStates.Idle.pure[F]

    implicit def doneIdleDone[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Done.type, ProtocolStates.Idle.type, ProtocolStates.Done.type] =
      (_, _, _) => ProtocolStates.Done.pure[F]
  }

}
