package co.topl.networking.typedprotocols

import cats.data.{Chain, EitherT}
import cats.effect.Resource
import cats.effect.kernel.{Async, Sync}
import cats.effect.std.Queue
import cats.implicits._
import co.topl.networking.{NetworkTypeTag, Parties, Party}

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
   * Produces an "Applier" which is an object which receives messages of "Any" type and attempts to apply them to the
   * current protocol state.
   * @param initialState The initial state of the protocol
   */
  def applier[S: NetworkTypeTag](
    initialState:    S
  )(implicit asyncF: Async[F]): Resource[F, MessageApplier] =
    (
      Resource.eval(Queue.unbounded[F, (Any, NetworkTypeTag[_])]),
      Resource.eval(Queue.unbounded[F, (Any, NetworkTypeTag[_])])
    ).tupled
      .flatMap { case (aQueue, bQueue) =>
        var state: Any = initialState
        var typeTag: NetworkTypeTag[_] = implicitly[NetworkTypeTag[S]]
        Async[F]
          .background(
            Sync[F]
              .delay(
                transitions
                  .find(_.inStateTypeTag == typeTag)
                  .get
                  .stateAgency
                  .agent
                  .get
              )
              .flatMap { party =>
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
                    .semiflatMap { case (newState, newTypeTag) =>
                      Sync[F]
                        .delay {
                          state = newState
                          typeTag = newTypeTag
                        }
                        .as(newState)
                    }
                    .leftMap(e => new IllegalStateException(e.toString)) // TODO
                    .rethrowT
                }
              }
              .foreverM
          )
          .as(new MessageApplier {
            def apply[Message: NetworkTypeTag](message: Message, sender: Party): F[Unit] =
              sender match {
                case Parties.A => aQueue.offer((message, implicitly[NetworkTypeTag[Message]]))
                case Parties.B => bQueue.offer((message, implicitly[NetworkTypeTag[Message]]))
              }
          })
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
