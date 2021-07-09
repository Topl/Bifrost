package co.topl

import akka.actor.typed.ActorRef
import co.topl.nodeView.NodeViewHolder

package object settings {

  /** This is the case class for when NodeViewHolder actor is set up which makes Bifrost ready for forging */
  case class NodeViewReady(nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage])
}
