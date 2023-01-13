package co.topl.networking.typedprotocols

import cats.data.{Chain, EitherT, OptionT}
import cats.effect.Resource
import cats.effect.kernel.{Async, Sync}
import cats.effect.std.Queue
import cats.implicits._
import co.topl.networking._

/**
 * Captures the domain of state transitions for some typed protocol.  The domain is used by an "applier" to handle
 * the state transition for "Any" arbitrary input message.
 * @param localParty The party designation to the local node
 * @param transitions The state transitions available to this typed protocol.  The appropriate implicits are also
 *                    captured and stored for later comparison
 */
case class TypedProtocolInstance[F[_]] private (
  localParty:              Party,
  private val transitions: Chain[WrappedStateTransition[F]]
) {

  /**
   * Append the given state transition to this instance
   */
  def withTransition[Message: NetworkTypeTag, InState: NetworkTypeTag: StateAgency, OutState: NetworkTypeTag](
    transition: StateTransition[F, Message, InState, OutState]
  ): TypedProtocolInstance[F] =
    copy(transitions =
      transitions.append(
        WrappedStateTransition(
          transition,
          implicitly[NetworkTypeTag[Message]],
          implicitly[NetworkTypeTag[InState]],
          implicitly[NetworkTypeTag[OutState]],
          implicitly[StateAgency[InState]]
        )
      )
    )

  private def applyMessage[Message](
    message:                    Message,
    sender:                     Party,
    currentState:               Any,
    currentStateNetworkTypeTag: NetworkTypeTag[_]
  )(implicit
    syncF:          Sync[F],
    messageTypeTag: NetworkTypeTag[Message]
  ): F[Either[TypedProtocolTransitionFailure, (Any, NetworkTypeTag[Any])]] =
    EitherT
      .fromOption[F](
        transitions.find { case WrappedStateTransition(_, messageNetworkTypeTag, inStateNetworkTypeTag, _, _) =>
          messageTypeTag == messageNetworkTypeTag && inStateNetworkTypeTag == currentStateNetworkTypeTag
        },
        IllegalMessageState(message, currentState)
      )
      .ensure(MessageSenderNotAgent(sender): TypedProtocolTransitionFailure) { handler =>
        handler.stateAgency.asInstanceOf[StateAgency[Any]].agent.contains(sender)
      }
      .semiflatMap { case WrappedStateTransition(transition, _, _, outStateNetworkTypeTag, _) =>
        transition
          .asInstanceOf[StateTransition[F, Message, Any, Any]](
            message,
            currentState,
            localParty
          )
          .tupleRight(outStateNetworkTypeTag.asInstanceOf[NetworkTypeTag[Any]])
      }
      .value

  /**
   * Produces an "Applier" which enqueues each message.  A background fiber processes these queues in the background
   * depending on the current "agent".
   * @param initialState The initial state of the protocol
   */
  def applier[S: NetworkTypeTag](initialState: S)(implicit asyncF: Async[F]): Resource[F, MessageApplier] =
    for {
      aQueue <- Resource.eval(Queue.bounded[F, (Any, NetworkTypeTag[_])](16))
      bQueue <- Resource.eval(Queue.bounded[F, (Any, NetworkTypeTag[_])](16))
      _      <- Async[F].background(backgroundProcessor(initialState)(aQueue, bQueue))
    } yield new MessageApplier {

      def apply[Message: NetworkTypeTag](message: Message, sender: Party): F[Unit] = {
        val queue =
          sender match {
            case Parties.A => aQueue
            case Parties.B => bQueue
          }
        queue.offer((message, implicitly[NetworkTypeTag[Message]]))
      }
    }

  /**
   * A forever(ish)-running process which reads from each of the given message queues (depending on the current agent),
   * processes the message, updates the state, and continues.  The process exits when reaching a state that defines
   * no agent (or when cancelled)
   * @param initialState The initial state of the protocol
   * @param aQueue A queue of tuples (message, tag) sent by party A
   * @param bQueue A queue of tuples (message, tag) sent by party B
   */
  private def backgroundProcessor[S: NetworkTypeTag](
    initialState: S
  )(aQueue:       Queue[F, (Any, NetworkTypeTag[_])], bQueue: Queue[F, (Any, NetworkTypeTag[_])])(implicit
    asyncF:       Async[F]
  ) = {
    var state: Any = initialState
    var typeTag: NetworkTypeTag[_] = implicitly[NetworkTypeTag[S]]
    OptionT(
      OptionT(Sync[F].delay(transitions.find(_.inStateTypeTag == typeTag)))
        .getOrRaise(new IllegalStateException("Unable to find transition matching current state tag"))
        .map(_.stateAgency.agent)
    )
      .semiflatTap { party =>
        val queue =
          party match {
            case Parties.A => aQueue
            case Parties.B => bQueue
          }
        queue.take.flatMap { case (nextMessage, nextTypeTag) =>
          EitherT(
            applyMessage(nextMessage, party, state, typeTag)(
              implicitly,
              nextTypeTag.asInstanceOf[NetworkTypeTag[Any]]
            )
          )
            .leftMap(TypedProtocolTransitionFailureException(nextMessage, _))
            .rethrowT
            .map { case (newState, newTypeTag) =>
              state = newState
              typeTag = newTypeTag
              newState
            }
        }
      }
      .isDefined
      .iterateWhile(identity)
      .void
  }

  /**
   * Encapsulates an internal mutable protocol state and applies inbound messages to it, if possible.
   */
  trait MessageApplier {

    def apply[Message: NetworkTypeTag](message: Message, sender: Party): F[Unit]
  }

}

object TypedProtocolInstance {
  def apply[F[_]](localParty: Party): TypedProtocolInstance[F] = TypedProtocolInstance[F](localParty, Chain.empty)
}

case class WrappedStateTransition[F[_]](
  transition:      StateTransition[F, _, _, _],
  messageTypeTag:  NetworkTypeTag[_],
  inStateTypeTag:  NetworkTypeTag[_],
  outStateTypeTag: NetworkTypeTag[_],
  stateAgency:     StateAgency[_]
)

sealed trait TypedProtocolTransitionFailure

case class IllegalMessageState[Message](message: Message, currentState: Any) extends TypedProtocolTransitionFailure
case class MessageSenderNotAgent(sender: Party) extends TypedProtocolTransitionFailure
