package co.topl.http.api

import akka.actor.ActorRef
import akka.pattern.ask
import co.topl.nodeView.CurrentView
import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State

import scala.concurrent.Future

trait ApiRouteWithView extends ApiRoute {

  val nodeViewHolderRef: ActorRef

  type CV = CurrentView[History, State, MemPool]

  protected def viewAsync(): Future[CV] = (nodeViewHolderRef ? GetDataFromCurrentView()).mapTo[CV]

}
