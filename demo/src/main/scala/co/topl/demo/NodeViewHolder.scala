package co.topl.demo

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import cats.effect.kernel.Async
import cats.implicits._
import cats.{Defer, Monad}
import co.topl.algebras.BlockchainState
import co.topl.models.Box.Values
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

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

    def make[F[_]: Async: Defer: Monad](
      actorRef:        ActorRef[ReceivableMessage]
    )(implicit system: ActorSystem[_], timeout: Timeout): BlockchainState[F] =
      new BlockchainState[F] {

        import akka.actor.typed.scaladsl.AskPattern._

        def genesis: F[BlockV2] =
          withNodeView(nodeView => (nodeView, nodeView.genesisBlock))

        def canonicalHead: F[BlockV2] =
          withNodeView(nodeView =>
            (
              nodeView,
              if (nodeView.blocks.isEmpty) nodeView.genesisBlock
              else nodeView.blocks.valuesIterator.maxBy(_.headerV2.height)
            )
          )

        def append(blockV2: BlockV2): F[Unit] =
          withNodeView(nodeView => (nodeView.copy(blocks = nodeView.blocks + (blockV2.headerV2.id -> blockV2)), ()))

        def lookupBlock(id: TypedIdentifier): F[Option[BlockV2]] =
          withNodeView(nodeView => (nodeView, nodeView.blocks.get(id)))

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

        def lookupEta(epoch: Epoch): F[Option[Eta]] =
          withNodeView(nodeView => (nodeView, nodeView.etas.get(epoch)))

        def writeEta(epoch: Epoch, eta: Eta): F[Unit] =
          withNodeView(nodeView => (nodeView.copy(etas = nodeView.etas + (epoch -> eta)), ()))

        private def withNodeView[T](f: NodeView => (NodeView, T)): F[T] =
          Async[F].fromFuture(
            Defer[F].defer(actorRef.ask[T](Run[T](f, _)).pure[F])
          )
      }
  }
}

case class NodeView(
  genesisBlock:   BlockV2,
  blocks:         Map[TypedIdentifier, BlockV2] = Map.empty,
  relativeStakes: Map[Epoch, Map[TaktikosAddress, Ratio]] = Map.empty,
  registrations:  Map[Epoch, Map[TaktikosAddress, Box.Values.TaktikosRegistration]] = Map.empty,
  etas:           Map[Epoch, Eta] = Map.empty
)
