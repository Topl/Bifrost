package co.topl.http.api

import akka.actor.ActorRef
import akka.pattern.ask
import co.topl.attestation.Address
import co.topl.nodeView.CurrentView
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import io.circe.Json

import scala.concurrent.{ExecutionContext, Future}

trait ApiEndpointWithView extends ApiEndpoint {

  val nodeViewHolderRef: ActorRef

  type CV = CurrentView[History, State, MemPool]

  protected def viewAsync(f: CV => Json)(implicit ec: ExecutionContext): Future[Json] =
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CV].map(f)

  /** Helper function to ensure this node has the appropriate state to create a request raw transaction */
  protected def checkAddress(keys: Seq[Address], view: CV): Unit = {
    if (!view.state.hasTBR)
      throw new Exception("TokenBoxRegistry not defined for node")

    //YT NOTE - if nodeKeys not defined in settings file then node watches for all keys in a state update
    if (view.state.nodeKeys.isDefined && !keys.forall(key => view.state.nodeKeys.contains(key)))
      throw new Exception("Node not set to watch for specified public key")
  }
}
