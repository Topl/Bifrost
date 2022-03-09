package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.models.TypedIdentifier
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}

object BlockHeight {

  object ProtocolStates {
    case class None()
    case class Idle()
    case class Busy()
    case class Done()
  }

  object ProtocolMessages {
    case class Start()
    case class GetBlockIdAtHeight(height: Long)
    case class GetBlockIdAtHeightResponse(blockIdOpt: Option[TypedIdentifier])
    case class Done()
  }

  class StateTransitionsServer[F[_]: Applicative](blockIdRequested: Long => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getBlockIdAtHeightIdleBusy
      : StateTransition[F, ProtocolMessages.GetBlockIdAtHeight, ProtocolStates.Idle, ProtocolStates.Busy] =
      (message, _, _) => blockIdRequested(message.height).as(TypedProtocolState(Parties.A.some, ProtocolStates.Busy()))

    implicit val getBlockIdAtHeightResponse
      : StateTransition[F, ProtocolMessages.GetBlockIdAtHeightResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

  class StateTransitionsClient[F[_]: Applicative](blockIdReceived: Option[TypedIdentifier] => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getBlockIdAtHeightIdleBusy
      : StateTransition[F, ProtocolMessages.GetBlockIdAtHeight, ProtocolStates.Idle, ProtocolStates.Busy] =
      (_, _, _) => TypedProtocolState(Parties.A.some, ProtocolStates.Busy()).pure[F]

    implicit val getBlockIdAtHeightResponse
      : StateTransition[F, ProtocolMessages.GetBlockIdAtHeightResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (message, _, _) =>
        blockIdReceived(message.blockIdOpt).as(TypedProtocolState(Parties.B.some, ProtocolStates.Idle()))

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

}
