package co.topl.networking.typedprotocols

import cats.Monad
import cats.data.{Chain, EitherT}
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.implicits._
import co.topl.networking.Party

import scala.reflect.runtime.universe._

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
    (StateTransition[F, _, _, _], WeakTypeTag[_], WeakTypeTag[_], WeakTypeTag[_], StateAgency[_])
  ]
) {

  /**
   * Append the given state transition to this instance
   */
  def withTransition[Message: WeakTypeTag, InState: WeakTypeTag: StateAgency, OutState: WeakTypeTag](
    transition: StateTransition[F, Message, InState, OutState]
  ): TypedProtocolInstance[F] =
    copy(transitions =
      transitions.append(
        (
          transition,
          implicitly[WeakTypeTag[Message]],
          implicitly[WeakTypeTag[InState]],
          implicitly[WeakTypeTag[OutState]],
          implicitly[StateAgency[InState]]
        )
      )
    )

  private def applyMessage[Message: WeakTypeTag](
    message:                 Message,
    sender:                  Party,
    currentState:            Any,
    currentStateWeakTypeTag: WeakTypeTag[_]
  )(implicit monadF:         Monad[F]): F[Either[TypedProtocolTransitionFailure, (Any, WeakTypeTag[Any])]] =
    EitherT
      .fromOption[F](
        transitions.find { case (_, messageWeakTypeTag, inStateWeakTypeTag, _, _) =>
          implicitly[WeakTypeTag[Message]] == messageWeakTypeTag && inStateWeakTypeTag == currentStateWeakTypeTag
        },
        IllegalMessageState(message, currentState)
      )
      .ensure(MessageSenderNotAgent(sender): TypedProtocolTransitionFailure) { handler =>
        handler._5.asInstanceOf[StateAgency[Any]].agent.contains(sender)
      }
      .semiflatMap { case (transition, _, _, outStateWeakTypeTag, _) =>
        transition
          .asInstanceOf[StateTransition[F, Message, Any, Any]](
            message,
            currentState,
            localParty
          )
          .tupleRight(outStateWeakTypeTag.asInstanceOf[WeakTypeTag[Any]])
      }
      .value

  /**
   * Produces an "Applier" which is an object which receives messages of "Any" type and attempts to apply them to the
   * current protocol state.
   * @param initialState The initial state of the protocol
   */
  def applier[S: WeakTypeTag](
    initialState:    S
  )(implicit monadF: Monad[F], syncF: Sync[F]): F[MessageApplier] =
    Ref
      .of[F, (Any, WeakTypeTag[_])]((initialState, implicitly[WeakTypeTag[S]]))
      .map(ref =>
        new MessageApplier {

          def apply[Message: WeakTypeTag](
            message: Message,
            sender:  Party
          ): F[Either[TypedProtocolTransitionFailure, Any]] =
            EitherT(ref.get.flatMap { case (s, sWeakTypeTag) =>
              applyMessage[Message](message, sender, s, sWeakTypeTag)
            })
              .semiflatTap(ref.set)
              .map(_._1)
              .value
        }
      )

  /**
   * Encapsulates an internal mutable protocol state and applies inbound messages to it, if possible.
   */
  trait MessageApplier {

    def apply[Message: WeakTypeTag](
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
