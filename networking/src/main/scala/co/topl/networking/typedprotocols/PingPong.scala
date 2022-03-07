package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}

object PingPong {

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

  object StateTransitions {

    implicit def startNoneIdle[F[_]: Applicative]: StateTransition[F, Messages.Start, States.None, States.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, States.Idle()).pure[F]

    implicit def pingIdleBusy[F[_]: Applicative]: StateTransition[F, Messages.Ping, States.Idle, States.Busy] =
      (_, _, _) => TypedProtocolState(Parties.A.some, States.Busy()).pure[F]

    implicit def pongBusyIdle[F[_]: Applicative]: StateTransition[F, Messages.Pong, States.Busy, States.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, States.Idle()).pure[F]

    implicit def doneIdleDone[F[_]: Applicative]: StateTransition[F, Messages.Done, States.Idle, States.Done] =
      (_, _, _) => TypedProtocolState(none, States.Done()).pure[F]
  }

}
