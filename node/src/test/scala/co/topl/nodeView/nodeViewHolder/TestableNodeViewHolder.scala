package co.topl.nodeView.nodeViewHolder

import akka.Done
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import co.topl.nodeView.{NodeView, NodeViewHolder}

import scala.concurrent.Await
import scala.concurrent.duration._

object TestableNodeViewHolder {
  import akka.actor.typed.scaladsl.AskPattern._
  implicit val timeout: Timeout = Timeout(10.seconds)

  def nodeViewOf(
    nodeViewHolder:  ActorRef[NodeViewHolder.ReceivableMessage]
  )(implicit system: ActorSystem[_]): NodeView =
    Await.result(
      nodeViewHolder.ask[NodeView](NodeViewHolder.ReceivableMessages.GetWritableNodeView),
      10.seconds
    )

  def setNodeView(nodeViewHolder: ActorRef[NodeViewHolder.ReceivableMessage], nodeView: NodeView)(implicit
    system:                       ActorSystem[_]
  ): Unit =
    Await.result(
      nodeViewHolder.ask[Done](NodeViewHolder.ReceivableMessages.SetWritableNodeView(nodeView, _)),
      10.seconds
    )
}
