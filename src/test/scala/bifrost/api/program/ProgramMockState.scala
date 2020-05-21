package bifrost.api.program

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.forging.Forger
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box._
import bifrost.network.message._
import bifrost.network.peer.PeerManager
import bifrost.network._
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.{BifrostState, BifrostStateChanges}
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import com.google.common.primitives.Ints
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

trait ProgramMockState extends BifrostGenerators {


  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef
  protected val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(BifrostSyncInfoMessageSpec)
  //p2p
  lazy val upnp = new UPnP(settings)

  private lazy val basicSpecs =
    Seq(
      GetPeersSpec,
      PeersSpec,
      InvSpec,
      RequestModifierSpec,
      ModifiersSpec
    )

  lazy val messagesHandler: MessageHandler = MessageHandler(basicSpecs ++ additionalMessageSpecs)

  val peerManagerRef: ActorRef = actorSystem.actorOf(Props(classOf[PeerManager], settings))

  val nProps = Props(classOf[NetworkController], settings, messagesHandler, upnp, peerManagerRef)
  val networkController: ActorRef = actorSystem.actorOf(nProps, "networkController")

  val forger: ActorRef = actorSystem.actorOf(Props(classOf[Forger], settings, nodeViewHolderRef))

  val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostLocalInterface], nodeViewHolderRef, forger, settings)
  )

  val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[BifrostNodeViewSynchronizer],
      networkController,
      nodeViewHolderRef,
      localInterface,
      BifrostSyncInfoMessageSpec)
  )

  implicit val timeout: Timeout = Timeout(10.seconds)

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/program/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  protected def view() = Await.result(
    (nodeViewHolderRef ? GetCurrentView)
      .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10.seconds)

  def manuallyApplyBoxes(boxes: Set[BifrostBox], version: Int): Unit = {
    // Manually manipulate state
    val boxSC = BifrostStateChanges(Set(),
      boxes,
      System.currentTimeMillis())

    view().state.applyChanges(boxSC, Ints.toByteArray(version)).get
  }

  val publicKey = "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
  val prop = PublicKey25519Proposition(Base58.decode(publicKey).get)

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
