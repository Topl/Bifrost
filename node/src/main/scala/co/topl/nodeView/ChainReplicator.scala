package co.topl.nodeView

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.network.Broadcast
import co.topl.network.NetworkController.ReceivableMessages.SendToNetwork
import co.topl.network.message.{InvData, InvSpec, Message}
import co.topl.nodeView
import co.topl.nodeView.NodeViewHolder.Events.SemanticallySuccessfulModifier
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.TimeProvider
import org.slf4j.Logger

import scala.collection.immutable.TreeSet
import scala.concurrent.Future
import scala.util.{Failure, Success}

object ChainReplicator {

  val actorName = "ChainReplicator"

  sealed abstract class ReceivableMessage

  object ReceivableMessages {

    private[nodeView] case class AppendBlock(block: Block) extends ReceivableMessage

    private[nodeView] case object Rollback extends ReceivableMessage

    private[nodeView] case object CheckDatabaseComplete extends ReceivableMessage

    private[nodeView] case class StartReplicating(replyTo: ActorRef[Done]) extends ReceivableMessage

    private[nodeView] case class StopReplicating(replyTo: ActorRef[Done]) extends ReceivableMessage

    private[nodeView] case class Terminate(reason: Throwable) extends ReceivableMessage
  }

  def apply(
    nodeViewHodlerRef:  ActorRef[NodeViewHolder.ReceivableMessage],
    replicateOnStartup: Boolean
  ): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      context.system.eventStream.tell(
        EventStream.Subscribe[SemanticallySuccessfulModifier[Block]](
          context.messageAdapter(block => ReceivableMessages.AppendBlock(block.modifier))
        )
      )
      context.log.info(s"${Console.GREEN}Chain replicator initializing${Console.RESET}")

      context.pipeToSelf(checkDatabase())(
        _.fold(ReceivableMessages.Terminate, _ => ReceivableMessages.CheckDatabaseComplete)
      )

      new ChainReplicator(nodeViewHodlerRef).uninitialized(replicateOnStartup)
    }

  /**
   * Check if the database is available
   */
  private def checkDatabase(): Future[Done] =
    //TODO: Jing - Check database
    Future.successful(Done)
}

private class ChainReplicator(nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage])(implicit
  context:                                       ActorContext[ChainReplicator.ReceivableMessage]
) {
  implicit private val log: Logger = context.log

  import ChainReplicator._

  def uninitialized(replicateWhenReady: Boolean): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartReplicating(replyTo) =>
        replyTo.tell(Done)
        uninitialized(replicateWhenReady = true)
      case ReceivableMessages.StopReplicating(replyTo) =>
        replyTo.tell(Done)
        uninitialized(replicateWhenReady = false)
      case ReceivableMessages.CheckDatabaseComplete =>
        log.info(s"${Console.GREEN}Chain replicator is ready${Console.RESET}")
        if (replicateWhenReady) {
          context.self.tell(ReceivableMessages.StartReplicating(context.system.ignoreRef))
          active
        } else {
          paused
        }
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  def paused: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartReplicating(replyTo) =>
        replyTo.tell(Done)
        active
      case ReceivableMessages.StopReplicating(replyTo) =>
        replyTo.tell(Done)
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  def active: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartReplicating(replyTo) =>
        replyTo.tell(Done)
        Behaviors.same
      case ReceivableMessages.StopReplicating(replyTo) =>
        replyTo.tell(Done)
        paused
      case ReceivableMessages.AppendBlock(block) =>
        //TODO: Jing - send new block to the database
        log.info(s"${Console.GREEN}=============\n$block\n=============${Console.RESET}")
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

}
