package co.topl.networking.typedprotocols

import cats.Applicative
import cats.implicits._
import co.topl.models.TypedIdentifier
import co.topl.networking.{Parties, StateTransition, TypedProtocolState}

/**
 * A Typed Protocol which gossips blocks which are adopted by a server node.
 */
object BlockGossipProtocol {

  object ProtocolStates {
    case class None()
    case class Idle()
    case class Busy()
    case class Done()
  }

  object ProtocolMessages {
    case class Start()
    case class GetUpdates()
    case class Update(blockId: TypedIdentifier)
    case class Done()
  }

  /**
   * Provides state transitions from the perspective of the "server"
   * @param updatesRequested Signals that the client requested a stream of updates
   */
  class StateTransitionsServer[F[_]: Applicative](updatesRequested: () => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getUpdatesIdleBusy
      : StateTransition[F, ProtocolMessages.GetUpdates, ProtocolStates.Idle, ProtocolStates.Busy] =
      (_, _, _) => updatesRequested().as(TypedProtocolState(Parties.A.some, ProtocolStates.Busy()))

    implicit val updateBusyBusy: StateTransition[F, ProtocolMessages.Update, ProtocolStates.Busy, ProtocolStates.Busy] =
      (_, protocolInState, _) => protocolInState.pure[F]

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

  /**
   * Provides state transitions from the perspective of the "client"
   * @param updateReceived Signals that the server has gossiped a new block ID
   */
  class StateTransitionsClient[F[_]: Applicative](updateReceived: TypedIdentifier => F[Unit]) {

    implicit val startNoneIdle: StateTransition[F, ProtocolMessages.Start, ProtocolStates.None, ProtocolStates.Idle] =
      (_, _, _) => TypedProtocolState(Parties.B.some, ProtocolStates.Idle()).pure[F]

    implicit val getUpdatesIdleBusy
      : StateTransition[F, ProtocolMessages.GetUpdates, ProtocolStates.Idle, ProtocolStates.Busy] =
      (_, _, _) => TypedProtocolState(Parties.A.some, ProtocolStates.Busy()).pure[F]

    implicit val updateBusyBusy: StateTransition[F, ProtocolMessages.Update, ProtocolStates.Busy, ProtocolStates.Busy] =
      (message, protocolInState, _) => updateReceived(message.blockId).as(protocolInState)

    implicit val doneIdleDone: StateTransition[F, ProtocolMessages.Done, ProtocolStates.Idle, ProtocolStates.Done] =
      (_, _, _) => TypedProtocolState(none, ProtocolStates.Done()).pure[F]
  }

}
