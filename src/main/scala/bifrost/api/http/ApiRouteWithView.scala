package bifrost.api.http

import akka.actor.ActorRef
import akka.pattern.ask
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import bifrost.nodeView.GenericNodeViewHolder.CurrentView
import bifrost.state.State
import bifrost.wallet.Wallet

import scala.concurrent.Future

trait ApiRouteWithView extends ApiRoute {

  val nodeViewHolderRef: ActorRef

  private def actOnCurrentView(v: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = v

  protected def viewAsync(): Future[CurrentView[History, State, Wallet, MemPool]] =
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CurrentView[History, State, Wallet, MemPool]]

}
