package co.topl.interpreters

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ConsensusState
import co.topl.models.Box.Values
import co.topl.models._
import co.topl.models.utility.Ratio

object NodeViewHolder {
  sealed abstract class ReceivableMessage

  case class Run[T](f: NodeView => (NodeView, T), replyTo: ActorRef[T]) extends ReceivableMessage {

    private[NodeViewHolder] def runAndTell(nodeView: NodeView) = {
      val (newState, res) = f(nodeView)
      replyTo.tell(res)
      newState
    }
  }

  def apply(nodeView: NodeView): Behavior[ReceivableMessage] =
    Behaviors.receiveMessage { case run: Run[_] => apply(run.runAndTell(nodeView)) }

  object StateEval {

    def make[F[_]: Async](
      actorRef:        ActorRef[ReceivableMessage]
    )(implicit system: ActorSystem[_], timeout: Timeout): ConsensusState[F] =
      new ConsensusState[F] {

        import akka.actor.typed.scaladsl.AskPattern._

        def lookupRelativeStake(epoch: Epoch)(address: TaktikosAddress): F[Option[Ratio]] =
          withNodeView(nodeView => (nodeView, nodeView.relativeStakes.get(epoch).flatMap(_.get(address))))

        def writeRelativeStakes(epoch: Epoch, relativeStakes: Map[TaktikosAddress, Ratio]): F[Unit] =
          withNodeView(nodeView =>
            (nodeView.copy(relativeStakes = nodeView.relativeStakes + (epoch -> relativeStakes)), ())
          )

        def foldRelativeStakes[S](epoch: Epoch)(s: S)(f: (S, (TaktikosAddress, Ratio)) => F[S]): F[S] =
          withNodeView(nodeView => (nodeView, nodeView.relativeStakes(epoch)))
            .flatMap(_.to(LazyList).foldLeftM(s)(f))

        def lookupRegistration(epoch: Epoch)(address: TaktikosAddress): F[Option[Values.TaktikosRegistration]] =
          withNodeView(nodeView => (nodeView, nodeView.registrations.get(epoch).flatMap(_.get(address))))

        def writeRegistrations(
          epoch:         Epoch,
          registrations: Map[TaktikosAddress, Values.TaktikosRegistration]
        ): F[Unit] =
          withNodeView(nodeView =>
            (nodeView.copy(registrations = nodeView.registrations + (epoch -> registrations)), ())
          )

        def foldRegistrations[S](epoch: Epoch)(s: S)(
          f:                            (S, (TaktikosAddress, Values.TaktikosRegistration)) => F[S]
        ): F[S] =
          withNodeView(nodeView => (nodeView, nodeView.registrations(epoch)))
            .flatMap(_.to(LazyList).foldLeftM(s)(f))

        private def withNodeView[T](f: NodeView => (NodeView, T)): F[T] =
          Async[F].fromFuture(
            actorRef.ask[T](Run[T](f, _)).pure[F]
          )

      }
  }
}

case class NodeView(
  relativeStakes: Map[Epoch, Map[TaktikosAddress, Ratio]] = Map.empty,
  registrations:  Map[Epoch, Map[TaktikosAddress, Box.Values.TaktikosRegistration]] = Map.empty
)
