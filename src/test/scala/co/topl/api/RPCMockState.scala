package co.topl.api

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import co.topl.attestation.PrivateKeyCurve25519
import co.topl.consensus.{Forger, ForgerRef, KeyRing}
import co.topl.crypto.KeyfileCurve25519
import co.topl.http.HttpService
import co.topl.http.api.ApiEndpoint
import co.topl.http.api.endpoints._
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{CurrentView, NodeViewHolder, NodeViewHolderRef}
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.CoreGenerators
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

trait RPCMockState extends AnyWordSpec
  with CoreGenerators
  with ScalatestRouteTest {

  override def createActorSystem(): ActorSystem = ActorSystem(settings.network.agentName)

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // save environment into a variable for reference throughout the application
  protected val appContext = new AppContext(settings, StartupOpts.empty, None)

  // Create Bifrost singleton actors
  protected val forgerRef: ActorRef = ForgerRef(Forger.actorName, settings, appContext)
  protected val nodeViewHolderRef: ActorRef = NodeViewHolderRef(NodeViewHolder.actorName, settings, appContext)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  implicit val timeout: Timeout = Timeout(10.seconds)

  private val apiRoutes: Seq[ApiEndpoint] = Seq(
    UtilsApiEndpoint(settings.rpcApi, appContext),
    KeyManagementApiEndpoint(settings.rpcApi, appContext, forgerRef),
    NodeViewApiEndpoint(settings.rpcApi, appContext, nodeViewHolderRef),
    TransactionApiEndpoint(settings.rpcApi, appContext, nodeViewHolderRef),
    DebugApiEndpoint(settings.rpcApi, appContext, nodeViewHolderRef, forgerRef)
  )

  private val httpService = HttpService(apiRoutes, settings.rpcApi)
  val route: Route = httpService.compositeRoute

  val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing(settings.application.keyFileDir.get, KeyfileCurve25519)


  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  protected def view(): CurrentView[History, State, MemPool] = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]],
    10.seconds)
}
