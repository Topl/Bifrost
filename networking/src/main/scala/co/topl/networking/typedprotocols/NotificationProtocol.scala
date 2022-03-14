package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.models.TypedIdentifier
import co.topl.networking.Parties

trait NotificationProtocol[T] {

  /**
   * Provides state transitions from the perspective of the "server"
   * @param updatesRequested Signals that the client requested a stream of updates
   */
  class StateTransitionsServer[F[_]: Applicative](updatesRequested: () => F[Unit]) {

    implicit val startNoneBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Start.type,
      TypedProtocol.CommonStates.None.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (_, _, _) => TypedProtocolState(Parties.A.some, TypedProtocol.CommonStates.Busy).pure[F]

    implicit val pushBusyBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Push[T],
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (_, protocolInState, _) => protocolInState.pure[F]

    implicit val doneBusyDone: StateTransition[
      F,
      TypedProtocol.CommonMessages.Done.type,
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Done.type
    ] =
      (_, _, _) => TypedProtocolState(none, TypedProtocol.CommonStates.Done).pure[F]
  }

  /**
   * Provides state transitions from the perspective of the "client"
   * @param updateReceived Signals that the server has gossiped a new ID
   */
  class StateTransitionsClient[F[_]: Applicative](updateReceived: T => F[Unit]) {

    implicit val startNoneBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Start.type,
      TypedProtocol.CommonStates.None.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (_, _, _) => TypedProtocolState(Parties.A.some, TypedProtocol.CommonStates.Busy).pure[F]

    implicit val pushBusyBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Push[T],
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (message, protocolInState, _) => updateReceived(message.data).productR(protocolInState.pure[F])

    implicit val doneBusyDone: StateTransition[
      F,
      TypedProtocol.CommonMessages.Done.type,
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Done.type
    ] =
      (_, _, _) => TypedProtocolState(none, TypedProtocol.CommonStates.Done).pure[F]
  }
}

object NotificationProtocols {
  object Id extends NotificationProtocol[TypedIdentifier]
  val BlockAdoption = Id
  val Transaction = Id
}
