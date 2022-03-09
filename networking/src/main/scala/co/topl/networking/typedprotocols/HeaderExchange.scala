package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.models.{BlockHeaderV2, TypedIdentifier}
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}

object HeaderExchange {

  object ProtocolStates {
    case class None()

    case class Idle()

    case class Busy(id: TypedIdentifier)

    case class Done()
  }

  object ProtocolMessages {
    case class Start()

    case class GetBlockHeader(id: TypedIdentifier)

    case class GetBlockHeaderResponse(headerOpt: Option[BlockHeaderV2])

    case class Done()
  }

  class StateTransitionsServer[F[_]: Applicative](blockRequested: TypedIdentifier => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getBlockHeaderIdleBusy
      : StateTransition[F, ProtocolMessages.GetBlockHeader, ProtocolStates.Idle, ProtocolStates.Busy] =
      (message, _, _) =>
        blockRequested(message.id).as(TypedProtocolState(Parties.A.some, ProtocolStates.Busy(message.id)))

    implicit val getBlockHeaderResponseBusyIdle
      : StateTransition[F, ProtocolMessages.GetBlockHeaderResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

  class StateTransitionsClient[F[_]: Applicative](blockReceived: (TypedIdentifier, Option[BlockHeaderV2]) => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getBlockHeaderIdleBusy
      : StateTransition[F, ProtocolMessages.GetBlockHeader, ProtocolStates.Idle, ProtocolStates.Busy] =
      (message, _, _) => TypedProtocolState(Parties.A.some, ProtocolStates.Busy(message.id)).pure[F]

    implicit val getBlockHeaderResponseBusyIdle
      : StateTransition[F, ProtocolMessages.GetBlockHeaderResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (message, state, _) =>
        blockReceived(state.currentState.id, message.headerOpt)
          .as(TypedProtocolState(Parties.B.some, ProtocolStates.Idle()))

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

}
