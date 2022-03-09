package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.models.{Transaction, TypedIdentifier}
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}

object TransactionExchange {

  object ProtocolStates {
    case class None()

    case class Idle()

    case class Busy()

    case class Done()
  }

  object ProtocolMessages {
    case class Start()

    case class GetTransaction(id: TypedIdentifier)

    case class GetTransactionResponse(transactionOpt: Option[Transaction])

    case class Done()
  }

  class StateTransitionsServer[F[_]: Applicative](transactionRequested: TypedIdentifier => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getTransactionIdleBusy
      : StateTransition[F, ProtocolMessages.GetTransaction, ProtocolStates.Idle, ProtocolStates.Busy] =
      (message, _, _) => transactionRequested(message.id).as(TypedProtocolState(Parties.A.some, ProtocolStates.Busy()))

    implicit val transactionReceivedBusyIdle
      : StateTransition[F, ProtocolMessages.GetTransactionResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

  class StateTransitionsClient[F[_]: Applicative](transactionReceived: Option[Transaction] => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getTransactionIdleBusy
      : StateTransition[F, ProtocolMessages.GetTransaction, ProtocolStates.Idle, ProtocolStates.Busy] =
      (_, _, _) => TypedProtocolState(Parties.A.some, ProtocolStates.Busy()).pure[F]

    implicit val transactionReceivedBusyIdle
      : StateTransition[F, ProtocolMessages.GetTransactionResponse, ProtocolStates.Busy, ProtocolStates.Idle] =
      (message, _, _) =>
        transactionReceived(message.transactionOpt).as(TypedProtocolState(Parties.B.some, ProtocolStates.Idle()))

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

}
