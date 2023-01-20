package co.topl.nodeView

import akka.Done
import akka.actor.typed._
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._

object TestableNodeViewHolder {
  implicit val timeout: Timeout = Timeout(10.seconds)

  def nodeViewOf(
    nodeViewHolder: ActorRef[NodeViewHolder.ReceivableMessage]
  )(implicit system: ActorSystem[_]): NodeView =
    Await.result(
      nodeViewHolder.ask[NodeView](NodeViewHolder.ReceivableMessages.GetWritableNodeView),
      10.seconds
    )

  def setNodeView(nodeViewHolder: ActorRef[NodeViewHolder.ReceivableMessage], f: NodeView => NodeView)(implicit
    system: ActorSystem[_]
  ): Unit =
    Await.result(
      nodeViewHolder.ask[Done](NodeViewHolder.ReceivableMessages.ModifyNodeView(f, _)),
      10.seconds
    )
}
