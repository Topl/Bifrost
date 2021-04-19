package co.topl.api

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import co.topl.akkahttprpc.ThrowableSupport.Standard._
import co.topl.consensus.{ActorForgerInterface, ActorKeyManagerInterface, Forger, ForgerRef}
import co.topl.http.HttpService
import co.topl.rpc.ToplRpcServer
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.{ActorNodeViewHolderInterface, CurrentView, NodeViewHolder, NodeViewHolderRef}
import co.topl.settings.{AppContext, AppSettings, StartupOpts}
import co.topl.utils.GenesisGenerators
import org.scalatest.wordspec.AnyWordSpec

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

trait RPCMockState extends AnyWordSpec with GenesisGenerators with ScalatestRouteTest {

  val tempFile: File = createTempFile

  val rpcSettings: AppSettings = settings.copy(
    application = settings.application.copy(
      dataDir = Some(tempFile.getPath + "data")
    )
  )

  val genesisState: State = genesisState(rpcSettings)

  //TODO Fails when using rpcSettings
  override def createActorSystem(): ActorSystem = ActorSystem(settings.network.agentName)

  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */
  // save environment into a variable for reference throughout the application
  protected val appContext = new AppContext(rpcSettings, StartupOpts.empty, None)

  // Create Bifrost singleton actors
  protected val forgerRef: ActorRef = ForgerRef(Forger.actorName, rpcSettings, appContext)
  protected val nodeViewHolderRef: ActorRef = NodeViewHolderRef(NodeViewHolder.actorName, rpcSettings, appContext)
  /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

  implicit val timeout: Timeout = Timeout(10.seconds)

  val rpcServer: ToplRpcServer = {
    val forgerInterface = new ActorForgerInterface(forgerRef)
    val keyManagerInterface = new ActorKeyManagerInterface(forgerRef)
    val nodeViewHolderInterface = new ActorNodeViewHolderInterface(nodeViewHolderRef)
    import co.topl.rpc.handlers._
    new ToplRpcServer(
      ToplRpcHandlers(
        new DebugRpcHandlerImpls(nodeViewHolderInterface, keyManagerInterface),
        new UtilsRpcHandlerImpls,
        new NodeViewRpcHandlerImpls(appContext, nodeViewHolderInterface),
        new TransactionRpcHandlerImpls(nodeViewHolderInterface),
        new AdminRpcHandlerImpls(forgerInterface, keyManagerInterface)
      ),
      appContext
    )
  }

  private val httpService = HttpService(rpcSettings.rpcApi, rpcServer)
  val route: Route = httpService.compositeRoute

  def httpPOST(jsonRequest: ByteString): HttpRequest =
    HttpRequest(
      HttpMethods.POST,
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))

  protected def view(): CurrentView[History, State, MemPool] =
    Await.result((nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]], 10.seconds)
}
