package bifrost.http.api

import akka.actor.ActorRef
import akka.pattern.ask
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.nodeView.CurrentView
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import bifrost.state.State
import bifrost.wallet.Wallet

import scala.concurrent.Future

trait ApiRouteWithView extends ApiRoute {

  val nodeViewHolderRef: ActorRef

  type CV = CurrentView[History, State, Wallet, MemPool]

  private def actOnCurrentView(v: CV): CV = v

  protected def viewAsync(): Future[CV] =
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CV]

}
