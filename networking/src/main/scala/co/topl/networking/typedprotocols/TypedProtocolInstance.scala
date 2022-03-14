package co.topl.networking.typedprotocols

import cats.Monad
import cats.data.{Chain, EitherT}
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.implicits._
import co.topl.networking.Party

import scala.reflect.ClassTag

case class TypedProtocolInstance[F[_]] private (
  localParty:              Party,
  private val transitions: Chain[(StateTransition[F, _, _, _], ClassTag[_], ClassTag[_], ClassTag[_])]
) {

  def withTransition[Message: ClassTag, InState: ClassTag, OutState: ClassTag](
    transition: StateTransition[F, Message, InState, OutState]
  ): TypedProtocolInstance[F] =
    copy(transitions =
      transitions.append(
        (transition, implicitly[ClassTag[Message]], implicitly[ClassTag[InState]], implicitly[ClassTag[OutState]])
      )
    )

  private def applyMessage[Message: ClassTag](
    message:              Message,
    sender:               Party,
    currentState:         TypedProtocolState[_],
    currentStateClassTag: ClassTag[_]
  )(implicit monadF: Monad[F]): F[Either[TypedProtocolTransitionFailure, (TypedProtocolState[Any], ClassTag[Any])]] =
    EitherT
      .fromOption[F](
        transitions.find { case (_, messageClassTag, inStateClassTag, _) =>
          implicitly[ClassTag[Message]] == messageClassTag && inStateClassTag == currentStateClassTag
        },
        IllegalMessageState(message, currentState)
      )
      .ensure(MessageSenderNotAgent(sender): TypedProtocolTransitionFailure)(_ =>
        currentState.currentAgent.contains(sender)
      )
      .semiflatMap { case (transition, _, _, outStateClassTag) =>
        transition
          .asInstanceOf[StateTransition[F, Message, Any, Any]](
            message,
            currentState.asInstanceOf[TypedProtocolState[Any]],
            localParty
          )
          .tupleRight(outStateClassTag.asInstanceOf[ClassTag[Any]])
      }
      .value

  def applier[S: ClassTag](
    initialState:    TypedProtocolState[S]
  )(implicit monadF: Monad[F], syncF: Sync[F]): F[MessageApplier] =
    Ref
      .of[F, (TypedProtocolState[_], ClassTag[_])]((initialState, implicitly[ClassTag[S]]))
      .map(ref =>
        new MessageApplier {

          def apply[Message: ClassTag](
            message: Message,
            sender:  Party
          ): F[Either[TypedProtocolTransitionFailure, TypedProtocolState[Any]]] =
            EitherT(ref.get.flatMap { case (s, sClassTag) =>
              applyMessage[Message](message, sender, s, sClassTag)
            })
              .semiflatTap(ref.set)
              .map(_._1)
              .value
        }
      )

  trait MessageApplier {

    def apply[Message: ClassTag](
      message: Message,
      sender:  Party
    ): F[Either[TypedProtocolTransitionFailure, TypedProtocolState[Any]]]
  }

}

object TypedProtocolInstance {
  def apply[F[_]](localParty: Party): TypedProtocolInstance[F] = TypedProtocolInstance[F](localParty, Chain.empty)
}

sealed trait TypedProtocolTransitionFailure

case class IllegalMessageState[Message](message: Message, currentState: TypedProtocolState[_])
    extends TypedProtocolTransitionFailure
case class MessageSenderNotAgent(sender: Party) extends TypedProtocolTransitionFailure
