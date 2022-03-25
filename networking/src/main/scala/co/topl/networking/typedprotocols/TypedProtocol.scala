package co.topl.networking.typedprotocols

import cats.implicits._
import co.topl.networking.{Parties, Party}

/**
 * A “Typed Protocol” is a specialized state machine with a few extra restrictions.  A typed protocol is a 2-party
 * exchange in which both parties share the exact same protocol state, but only one party has
 * agency/permission to send the next message.
 *
 * https://docs.cardano.org/explore-cardano/cardano-network/about-the-cardano-network/#utilizingmini-protocols
 */
object TypedProtocol {
  def apply[F[_]]: TransitionStep1[F] = new TransitionStep1[F]

  /**
   * A builder/helper for constructing a typesafe state-transition for a message handler.
   */
  final class TransitionStep1[F[_]] private[TypedProtocol] {
    def apply(localParty: Party): TransitionStep2 = new TransitionStep2(localParty)

    final class TransitionStep2 private[TypedProtocol] (localParty: Party) {

      def apply[Message](message: Message): TransitionStep3[Message] =
        new TransitionStep3[Message](message)

      final class TransitionStep3[Message] private[TypedProtocol] (message: Message) {

        def apply[InState](protocolInState: InState): TransitionStep4[InState] =
          new TransitionStep4[InState](protocolInState)

        final class TransitionStep4[InState] private[TypedProtocol] (protocolInState: InState) {

          def nextState[OutState](implicit
            handler: StateTransition[F, Message, InState, OutState]
          ): F[OutState] =
            handler.apply(message, protocolInState, localParty)
        }
      }
    }
  }

  object CommonStates {
    case object None
    case object Idle
    case object Busy
    case object Done
  }

  object CommonMessages {
    case object Start
    case class Get[Query](query: Query)
    case class Response[T](dataOpt: Option[T])
    case class Push[T](data: T)
    case object Done
  }
}

/**
 * Represents a function that transforms some specific state type using some specific message
 * and outputting some specific new state.
 */
trait StateTransition[F[_], Message, InState, OutState] {

  def apply(
    message:         Message,
    protocolInState: InState,
    localParty:      Party
  ): F[OutState]
}

/**
 * Represents a pairing of ProtocolState to Active Agent of that state
 */
trait StateAgency[ProtocolState] {
  def agent: Option[Party]
}

object StateAgency {

  def apply[ProtocolState](a: Option[Party]): StateAgency[ProtocolState] =
    new StateAgency[ProtocolState] {
      def agent: Option[Party] = a
    }

  def alwaysA[ProtocolState]: StateAgency[ProtocolState] = StateAgency[ProtocolState](Parties.A.some)
  def alwaysB[ProtocolState]: StateAgency[ProtocolState] = StateAgency[ProtocolState](Parties.B.some)
  def noAgent[ProtocolState]: StateAgency[ProtocolState] = StateAgency[ProtocolState](none)

  trait CommonStateAgency {
    implicit val stateAgentStart: StateAgency[TypedProtocol.CommonStates.None.type] = StateAgency.alwaysB
    implicit val stateAgentIdle: StateAgency[TypedProtocol.CommonStates.Idle.type] = StateAgency.alwaysB
    implicit val stateAgentBusy: StateAgency[TypedProtocol.CommonStates.Busy.type] = StateAgency.alwaysA
    implicit val stateAgentDone: StateAgency[TypedProtocol.CommonStates.Done.type] = StateAgency.noAgent
  }
}
