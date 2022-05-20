package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._

/**
 * A classification of TypedProtocol in which the server perpetually maintains agency to push information to the client.
 * @tparam T The type of data pushed to the client
 */
trait NotificationProtocol[T] {

  /**
   * Provides state transitions from the perspective of the "server"
   */
  class StateTransitionsServer[F[_]: Applicative](clientSentStart: () => F[Unit]) {
    implicit val stateAgentStart: StateAgency[TypedProtocol.CommonStates.None.type] = StateAgency.alwaysB
    implicit val stateAgentIdle: StateAgency[TypedProtocol.CommonStates.Idle.type] = StateAgency.alwaysB
    implicit val stateAgentBusy: StateAgency[TypedProtocol.CommonStates.Busy.type] = StateAgency.alwaysA
    implicit val stateAgentDone: StateAgency[TypedProtocol.CommonStates.Done.type] = StateAgency.noAgent

    implicit val startNoneBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Start.type,
      TypedProtocol.CommonStates.None.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (_, _, _) => clientSentStart().as(TypedProtocol.CommonStates.Busy)

    implicit val pushBusyBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Push[T],
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (_, _, _) => TypedProtocol.CommonStates.Busy.pure[F]

    implicit val doneBusyDone: StateTransition[
      F,
      TypedProtocol.CommonMessages.Done.type,
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Done.type
    ] =
      (_, _, _) => TypedProtocol.CommonStates.Done.pure[F]
  }

  /**
   * Provides state transitions from the perspective of the "client"
   * @param updateReceived Signals that the server has gossiped a new ID
   */
  class StateTransitionsClient[F[_]: Applicative](updateReceived: T => F[Unit]) {
    implicit val stateAgentStart: StateAgency[TypedProtocol.CommonStates.None.type] = StateAgency.alwaysB
    implicit val stateAgentIdle: StateAgency[TypedProtocol.CommonStates.Idle.type] = StateAgency.alwaysB
    implicit val stateAgentBusy: StateAgency[TypedProtocol.CommonStates.Busy.type] = StateAgency.alwaysA
    implicit val stateAgentDone: StateAgency[TypedProtocol.CommonStates.Done.type] = StateAgency.noAgent

    implicit val startNoneBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Start.type,
      TypedProtocol.CommonStates.None.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (_, _, _) => TypedProtocol.CommonStates.Busy.pure[F]

    implicit val pushBusyBusy: StateTransition[
      F,
      TypedProtocol.CommonMessages.Push[T],
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Busy.type
    ] =
      (message, _, _) => updateReceived(message.data).as(TypedProtocol.CommonStates.Busy)

    implicit val doneBusyDone: StateTransition[
      F,
      TypedProtocol.CommonMessages.Done.type,
      TypedProtocol.CommonStates.Busy.type,
      TypedProtocol.CommonStates.Done.type
    ] =
      (_, _, _) => TypedProtocol.CommonStates.Done.pure[F]
  }
}
