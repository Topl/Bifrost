package bifrost.api.program

import akka.actor.{ ActorRef, ActorSystem }
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ HttpEntity, HttpMethods, HttpRequest, MediaTypes }
import akka.pattern.ask
import akka.util.{ ByteString, Timeout }
import bifrost.crypto.{ FastCryptographicHash, PrivateKey25519 }
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import bifrost.nodeView.{ CurrentView, NodeViewHolderRef }
import bifrost.settings.BifrostContext
import bifrost.state.{ ProgramId, State }
import bifrost.wallet.Wallet
import bifrost.{ BifrostGenerators, state }
import com.google.common.primitives.Ints
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext }
import scala.reflect.io.Path
import scala.util.Try

trait ProgramMockState extends BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  // TODO: fix actor system creation with ScalatestRouteTest (using private for now)
  private implicit lazy val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  private implicit lazy val executionContext: ExecutionContext = actorSystem.dispatcher

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // save environment into a variable for reference throughout the application
  protected val bifrostContext = new BifrostContext(settings, None)

  // Create Bifrost singleton actors
  protected val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, bifrostContext)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

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

  def manuallyApplyBoxes(version: Int, boxes: Set[Box]): Unit = {
    // Manually manipulate state
    state.updateStorage(version, boxes, view().state)
  }

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
    )
  // Unlock Secrets
  val gw: Wallet = view().vault
  gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  val publicKey = "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
  val prop: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode(publicKeys("investor")).get)

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

  val stateBox: StateBox = StateBox(prop, 0L, programIdGen.sample.get, Map("a" -> 0, "b" -> 1).asJson)
  val codeBox: CodeBox = CodeBox(prop, 1L, programIdGen.sample.get, Seq("add = function(x,y) { a = x + y; return a }"), Map("add" -> Seq("Number", "Number")))
  val executionBox: ExecutionBox = ExecutionBox(prop, 2L, programIdGen.sample.get, Seq(stateBox.value), Seq(codeBox.value))
}
