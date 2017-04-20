package examples.bifrost.api.http

import akka.actor.ActorRef
import akka.pattern.ask
import examples.bifrost.history.BifrostHistory
import examples.bifrost.mempool.BifrostMemPool
import examples.bifrost.state.BifrostState
import examples.bifrost.wallet.BWallet
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.api.http.ApiRoute

import scala.concurrent.Future

trait ApiRouteWithView extends ApiRoute {

  val nodeViewHolderRef: ActorRef

  protected def viewAsync(): Future[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]] =
    (nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]]
}
