package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}

object PingPong {

  object ProtocolStates {
    case class None()
    case class Idle()
    case class Busy()
    case class Done()
  }

  object ProtocolMessages {
    case class Start()
    case class Ping()
    case class Pong()
    case class Done()
  }

  object StateTransitions {

    implicit def startNoneIdle[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit def pingIdleBusy[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Ping, ProtocolStates.Idle, ProtocolStates.Busy] =
      (_, _, _) => TypedProtocolState(Parties.A.some, ProtocolStates.Busy()).pure[F]

    implicit def pongBusyIdle[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Pong, ProtocolStates.Busy, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit def doneIdleDone[F[_]: Applicative]
      : StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

}
