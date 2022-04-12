package co.topl.networking.typedprotocols

import cats.Monad
import cats.data.{Chain, EitherT}
import cats.effect.Concurrent
import cats.effect.std.Semaphore
import cats.implicits._
import co.topl.networking.{NetworkTypeTag, Party}

/**
 * Captures the domain of state transitions for some typed protocol.  The domain is used by an "applier" to handle
 * the state transition for "Any" arbitrary input message.
 * @param localParty The party designation to the local node
 * @param transitions The state transitions available to this typed protocol.  The appropriate implicits are also
 *                    captured and stored for later comparison
 */
case class TypedProtocolInstance[F[_]] private (
  localParty: Party,
  private val transitions: Chain[
    (StateTransition[F, _, _, _], NetworkTypeTag[_], NetworkTypeTag[_], NetworkTypeTag[_], StateAgency[_])
  ]
) {

  /**
   * Append the given state transition to this instance
   */
  def withTransition[Message: NetworkTypeTag, InState: NetworkTypeTag: StateAgency, OutState: NetworkTypeTag](
    transition: StateTransition[F, Message, InState, OutState]
  ): TypedProtocolInstance[F] =
    copy(transitions =
      transitions.append(
        (
          transition,
          implicitly[NetworkTypeTag[Message]],
          implicitly[NetworkTypeTag[InState]],
          implicitly[NetworkTypeTag[OutState]],
          implicitly[StateAgency[InState]]
        )
      )
    )

  private def applyMessage[Message: NetworkTypeTag](
    message:                    Message,
    sender:                     Party,
    currentState:               Any,
    currentStateNetworkTypeTag: NetworkTypeTag[_]
  )(implicit
    monadF:         Monad[F],
    messageTypeTag: NetworkTypeTag[Message]
  ): F[Either[TypedProtocolTransitionFailure, (Any, NetworkTypeTag[Any])]] =
    EitherT
      .fromOption[F](
        transitions.find { case (_, messageNetworkTypeTag, inStateNetworkTypeTag, _, _) =>
          messageTypeTag == messageNetworkTypeTag && inStateNetworkTypeTag == currentStateNetworkTypeTag
        },
        IllegalMessageState(message, currentState)
      )
      .ensure(MessageSenderNotAgent(sender): TypedProtocolTransitionFailure) { handler =>
        handler._5.asInstanceOf[StateAgency[Any]].agent.contains(sender)
      }
      .semiflatMap { case (transition, _, _, outStateNetworkTypeTag, _) =>
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
    initialState:         S
  )(implicit concurrentF: Concurrent[F]): F[MessageApplier] =
    Semaphore[F](1)
      .map(semaphore =>
        new MessageApplier {
          private var state: Any = initialState
          private var typeTag: NetworkTypeTag[_] = implicitly[NetworkTypeTag[S]]

          def apply[Message: NetworkTypeTag](
            message: Message,
            sender:  Party
          ): F[Either[TypedProtocolTransitionFailure, Any]] = {
            for {
              _                      <- EitherT.liftF[F, TypedProtocolTransitionFailure, Unit](semaphore.acquire)
              (newState, newTypeTag) <- EitherT(applyMessage[Message](message, sender, state, typeTag))
              _ = state = newState
              _ = typeTag = newTypeTag
              _ <- EitherT.liftF[F, TypedProtocolTransitionFailure, Unit](semaphore.release)
            } yield newState
          }.value
        }
      )

  /**
   * Encapsulates an internal mutable protocol state and applies inbound messages to it, if possible.
   */
  trait MessageApplier {

    def apply[Message: NetworkTypeTag](
      message: Message,
      sender:  Party
    ): F[Either[TypedProtocolTransitionFailure, Any]]
  }

}

object TypedProtocolInstance {
  def apply[F[_]](localParty: Party): TypedProtocolInstance[F] = TypedProtocolInstance[F](localParty, Chain.empty)
}

sealed trait TypedProtocolTransitionFailure

case class IllegalMessageState[Message](message: Message, currentState: Any) extends TypedProtocolTransitionFailure
case class MessageSenderNotAgent(sender: Party) extends TypedProtocolTransitionFailure
