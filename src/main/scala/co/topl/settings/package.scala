package co.topl

import akka.actor.ActorRef

package object settings {

  case class NodeViewReady(nodeViewHolderRef: ActorRef)

}
