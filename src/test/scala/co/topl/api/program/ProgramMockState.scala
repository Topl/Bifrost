package co.topl.api.program

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import co.topl.BifrostGenerators
import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box._
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.{CurrentView, NodeViewHolderRef, state}
import co.topl.settings.AppContext
import co.topl.wallet.WalletActorManager
import io.circe.syntax._
import scorex.util.encode.Base58
import scorex.crypto.signatures.PublicKey

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
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
  protected val appContext = new AppContext(settings, None)

  // Create Bifrost singleton actors
  private val walletActorManagerRef: ActorRef = WalletActorManager.apply
  protected val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext, walletActorManagerRef)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  implicit val timeout: Timeout = Timeout(10.seconds)

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/program/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  protected def view(): CurrentView[History, State, MemPool] = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]],
    10.seconds)

  def directlyAddPBRStorage(version: Int, boxes: Seq[ProgramBox]): Unit = {
    // Manually manipulate state
    state.directlyAddPBRStorage(version, boxes, view().state)
  }

  lazy val (signSk, signPk) = sampleUntilNonEmpty(keyPairSetGen).head

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
    )

  val publicKey = "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
  val prop: PublicKey25519Proposition = PublicKey25519Proposition(publicKeys("investor"))

  val polyBoxes: Seq[TokenBox] = view().state.getTokenBoxes(prop).getOrElse(Seq())

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
