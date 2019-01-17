package bifrost.api.http

import akka.actor.ActorRef
import akka.pattern.ask
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
// import bifrost.api.http.ApiRoute

import scala.concurrent.Future

trait ApiRouteWithView extends ApiRoute {

  val nodeViewHolderRef: ActorRef

  protected def viewAsync(): Future[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]] =
    (nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]]
}