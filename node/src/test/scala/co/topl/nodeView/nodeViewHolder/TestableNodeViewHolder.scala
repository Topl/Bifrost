package co.topl.nodeView.nodeViewHolder

import co.topl.nodeView.{NodeView, NodeViewHolder}

object TestableNodeViewHolder {
  def nodeViewOf(nodeViewHolder: NodeViewHolder): NodeView = nodeViewHolder.nodeView

  def setNodeView(nodeViewHolder: NodeViewHolder, nodeView: NodeView): Unit = {
    nodeViewHolder.nodeView = nodeView
    nodeViewHolder.nodeViewWriter.nodeView = nodeView
  }
}
