package co.topl.nodeView

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.pattern.StatusReply

/**
 * Mock [[Behavior]] which accepts messages of type [[NodeViewHolder.ReceivableMessage]].
 */
object MockNodeViewHolderBehavior {

  /**
   * Instantiates a read-only behavior with a static internal [[ReadableNodeView]] state which can be accessed
   * via the [[NodeViewHolder.ReceivableMessages.Read]] message.
   * @param nodeView the read-only mock node view state
   * @return a [[Behavior]] instance which accepts [[NodeViewHolder.ReceivableMessage]] messages
   */
  def readOnly(nodeView: ReadableNodeView): Behavior[NodeViewHolder.ReceivableMessage] =
    Behaviors.receiveMessage {
      case read: NodeViewHolder.ReceivableMessages.Read[Any] =>
        read.replyTo.tell(StatusReply.success(read.f.apply(nodeView)))
        Behaviors.same
      case _: NodeViewHolder.ReceivableMessages.ModifyNodeView =>
        Behaviors.same
      case _: NodeViewHolder.ReceivableMessages.EliminateTransactions =>
        Behaviors.same
      case _: NodeViewHolder.ReceivableMessages.GetWritableNodeView =>
        Behaviors.same
      case _: NodeViewHolder.ReceivableMessages.WriteBlocks =>
        Behaviors.same
      case _: NodeViewHolder.ReceivableMessages.WriteTransactions =>
        Behaviors.same
    }
}
