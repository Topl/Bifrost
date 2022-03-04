package co.topl.networking

object TypedProtocol1 {
  def apply[F[_]]: TransitionStep1[F] = new TransitionStep1[F]

  class TransitionStep1[F[_]] private[TypedProtocol1] {
    def apply(localParty: Party): TransitionStep2 = new TransitionStep2(localParty)

    class TransitionStep2 private[TypedProtocol1] (localParty: Party) {

      def apply[Message](message: Message): TransitionStep3[Message] =
        new TransitionStep3[Message](message)

      class TransitionStep3[Message] private[TypedProtocol1] (message: Message) {

        def apply[InState](inState: InState): TransitionStep4[InState] =
          new TransitionStep4[InState](inState)

        class TransitionStep4[InState] private[TypedProtocol1] (inState: InState) {

          def nextState[OutState](implicit
            handler: StateTransition[F, Message, InState, OutState]
          ): F[TypedProtocolState[OutState]] =
            handler.apply(message, inState, localParty)
        }
      }
    }
  }
}

/**
 * Represents the current state of a Typed Protocol
 * @param currentAgent The party that currently has "agency" (the party expected to send the next message).
 *                     If None, assume the protocol has terminated.
 * @param currentState The current protocol-specific state
 * @tparam State The type of State
 */
case class TypedProtocolState[State](currentAgent: Option[Party], currentState: State)

/**
 * Represents a function that transforms some specific state type using some specific message
 * and outputting some specific new state.
 */
trait StateTransition[F[_], Message, InState, OutState] {

  def apply(
    message:    Message,
    inState:    InState,
    localParty: Party
  ): F[TypedProtocolState[OutState]]
}
