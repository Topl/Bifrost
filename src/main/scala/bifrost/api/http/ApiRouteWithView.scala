package bifrost.api.http

import akka.actor.ActorRef
import akka.pattern.ask
import bifrost.history.BifrostHistory
import bifrost.mempool.MemPool
import bifrost.nodeView.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.Wallet

import scala.concurrent.Future

trait ApiRouteWithView extends ApiRoute {

  val nodeViewHolderRef: ActorRef

  protected def viewAsync(): Future[CurrentView[BifrostHistory, BifrostState, Wallet, MemPool]] =
    (nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[BifrostHistory, BifrostState, Wallet, MemPool]]
}