package co.topl.api

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestActorRef
import akka.util.{ByteString, Timeout}
import co.topl.akkahttprpc.ThrowableSupport.Standard._
import co.topl.consensus._
import co.topl.http.HttpService
import co.topl.modifier.block.Block
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.ActorNodeViewHolderInterface
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.nodeViewHolder.TestableNodeViewHolder
import co.topl.nodeView.state.State
import co.topl.rpc.ToplRpcServer
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.{KeyFileTestHelper, NodeGenerators}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.DurationInt

trait RPCMockState
    extends AnyWordSpec
    with NodeGenerators
    with ScalatestRouteTest
    with BeforeAndAfterAll
    with KeyFileTestHelper {

  type BSI = BifrostSyncInfo
  type PMOD = Block
  type HIS = History
  type MP = MemPool
  type ST = State

  implicit val timeout: Timeout = Timeout(10.seconds)

  implicit protected val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(5.seconds)

  //TODO Fails when using rpcSettings
  override def createActorSystem(): ActorSystem = ActorSystem(settings.network.agentName)

  protected var appContext: AppContext = _

  // Create Bifrost singleton actors

  // NOTE: Some of these actors are TestActors in order to access the underlying instance so that we can manipulate
  //       the state of the underlying instance while testing. Use with caution
  protected var keyManagerRef: TestActorRef[KeyManager] = _
  protected var forgerRef: ActorRef = _

  protected var nodeViewHolderRef: TestActorRef[TestableNodeViewHolder] = _

  // Get underlying references
  protected var nvh: TestableNodeViewHolder = _
  protected var km: KeyManager = _

  var rpcServer: ToplRpcServer = _

  var route: Route = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    appContext = new AppContext(settings, StartupOpts.empty, None)

    keyManagerRef = TestActorRef(
      new KeyManager(settings, appContext)(system.getDispatcher, appContext.networkType.netPrefix)
    )
    forgerRef = ForgerRef[HIS, ST, MP](Forger.actorName, settings, appContext, keyManagerRef)

    nodeViewHolderRef = TestActorRef(
      new TestableNodeViewHolder(settings, appContext)(system.getDispatcher, appContext.networkType.netPrefix)
    )
    nvh = nodeViewHolderRef.underlyingActor
    km = keyManagerRef.underlyingActor

    // manipulate the underlying actor state
    nvh.updateNodeViewPublicAccessor(updatedState = Some(genesisState))
    km.context.become(km.receive(keyRing, Some(keyRing.addresses.head)))

    rpcServer = {
      val forgerInterface = new ActorForgerInterface(forgerRef)
      val keyManagerInterface = new ActorKeyManagerInterface(keyManagerRef)
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

    val httpService = HttpService(settings.rpcApi, rpcServer)
    route = httpService.compositeRoute
  }

  def httpPOST(jsonRequest: ByteString): HttpRequest =
    HttpRequest(
      HttpMethods.POST,
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))

  // this method returns modifiable instances of the node view components
  protected def view(): (History, State, MemPool) = nvh.nodeViewPublicAccessor
}
