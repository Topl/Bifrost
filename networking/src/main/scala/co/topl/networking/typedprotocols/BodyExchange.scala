package co.topl.networking.typedprotocols

import cats.Applicative
import cats.data.Chain
import cats.implicits._
import co.topl.models.TypedIdentifier
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}

object BodyExchange {

  object ProtocolStates {
    case class None()

    case class Idle()

    case class Busy()

    case class Done()
  }

  object ProtocolMessages {
    case class Start()

    case class GetBlockTransactionIds(id: TypedIdentifier)

    case class GetBlockTransactionIdsResponse(transactionIdsOpt: Option[Chain[TypedIdentifier]])

    case class Done()
  }

  class StateTransitionsServer[F[_]: Applicative](blockRequested: TypedIdentifier => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getBlockTransactionIdsIdleBusy
      : StateTransition[F, ProtocolMessages.GetBlockTransactionIds, ProtocolStates.Idle, ProtocolStates.Busy] =
      (message, _, _) => blockRequested(message.id).as(TypedProtocolState(Parties.A.some, ProtocolStates.Busy()))

    implicit val getBlockTransactionIdsResponseBusyIdle
      : StateTransition[F, ProtocolMessages.GetBlockTransactionIdsResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

  class StateTransitionsClient[F[_]: Applicative](transactionIdsReceived: Option[Chain[TypedIdentifier]] => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getBlockTransactionIdsIdleBusy
      : StateTransition[F, ProtocolMessages.GetBlockTransactionIds, ProtocolStates.Idle, ProtocolStates.Busy] =
      (_, _, _) => TypedProtocolState(Parties.A.some, ProtocolStates.Busy()).pure[F]

    implicit val getBlockTransactionIdsResponseBusyIdle
      : StateTransition[F, ProtocolMessages.GetBlockTransactionIdsResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (message, _, _) =>
        transactionIdsReceived(message.transactionIdsOpt).as(TypedProtocolState(Parties.B.some, ProtocolStates.Idle()))

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

}
