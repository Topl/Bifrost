package bifrost.api.program

import java.net.InetSocketAddress
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.BifrostGenerators
import bifrost.consensus.ForgerRef
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box._
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.network.message._
import bifrost.network.peer.{PeerFeature, PeerManagerRef}
import bifrost.network._
import bifrost.network.upnp
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import bifrost.nodeView.CurrentView
import bifrost.nodeView.{NodeViewHolderRef, NodeViewModifier}
import bifrost.settings.BifrostContext
import bifrost.state.{State, StateChanges}
import bifrost.utils.NetworkTimeProvider
import bifrost.wallet.Wallet
import com.google.common.primitives.Ints
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

trait ProgramMockState extends BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  // TODO: fix actor system creation with ScalatestRouteTest (using private for now)
  private implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  private implicit lazy val executionContext: ExecutionContext = actorSystem.dispatcher

  val timeProvider = new NetworkTimeProvider(settings.ntp)
  val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, timeProvider)

  protected val features: Seq[PeerFeature] = Seq()
  protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BifrostSyncInfoMessageSpec)
  //p2p
  private val upnpGateway: Option[upnp.Gateway] = if (settings.network.upnpEnabled) upnp.Gateway(settings.network) else None

  private lazy val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)
    val featureSerializers: PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap
    Seq(
      GetPeersSpec,
      new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects),
      invSpec,
      requestModifierSpec,
      modifiersSpec
    )
  }

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    settings.network.declaredAddress orElse {
      // TODO use available port on gateway instead settings.bindAddress.getPort
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, settings.network.bindAddress.getPort))
    }
  }

  val bifrostContext: BifrostContext = BifrostContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  val peerManagerRef: ActorRef = PeerManagerRef("peerManager", settings, bifrostContext)

  val networkControllerRef: ActorRef = NetworkControllerRef("networkController" ,settings.network, peerManagerRef, bifrostContext, peerManagerRef)

  val forgerRef: ActorRef = ForgerRef("forger", settings, nodeViewHolderRef)

  val nodeViewSynchronizer: ActorRef =
    NodeViewSynchronizerRef[Transaction, BifrostSyncInfo, BifrostSyncInfoMessageSpec.type, Block, History, MemPool](
      "nodeViewSynchronizer", networkControllerRef, nodeViewHolderRef,
      BifrostSyncInfoMessageSpec, settings.network, timeProvider, NodeViewModifier.modifierSerializers)

  implicit val timeout: Timeout = Timeout(10.seconds)

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/program/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  private def actOnCurrentView(v: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = v

  protected def view(): CurrentView[History, State, Wallet, MemPool] = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CurrentView[History, State, Wallet, MemPool]],
    10.seconds)


  def manuallyApplyBoxes(boxes: Set[Box], version: Int): Unit = {
    // Manually manipulate state
    val boxSC = StateChanges(Set(),
      boxes,
      System.currentTimeMillis())
    val versionId = ModifierId(Ints.toByteArray(version))

    view().state.applyChanges(boxSC, versionId).get
  }

  val publicKey = "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
  val prop: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode(publicKey).get)

  val polyBoxes = view()
    .vault
    .boxes()
    .filter(_.box.isInstanceOf[PolyBox])

  val fees: Map[String, Int] = Map(publicKey -> 500)

  val program: String =
    s"""
       |var a = 0
       |var b = 1
       |
       |add = function(x,y) {
       |  a = x + y
       |  return a
       |}
       |""".stripMargin

  val stateBox = StateBox(prop, 0L, UUID.nameUUIDFromBytes(StateBox.idFromBox(prop, 0L)), Map("a" -> 0, "b" -> 1).asJson)
  val codeBox = CodeBox(prop, 1L, UUID.nameUUIDFromBytes(CodeBox.idFromBox(prop, 1L)),
    Seq("add = function(x,y) { a = x + y; return a }"), Map("add" -> Seq("Number", "Number")))
  val executionBox = ExecutionBox(prop, 2L, UUID.nameUUIDFromBytes(ExecutionBox.idFromBox(prop, 2L)), Seq(stateBox.value), Seq(codeBox.id))
}
