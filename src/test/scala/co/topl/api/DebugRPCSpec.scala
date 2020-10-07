package co.topl.api

import akka.actor.ActorRef
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import co.topl.BifrostGenerators
import co.topl.nodeView.history.History
import co.topl.http.api.routes.DebugApiRoute
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.{CurrentView, NodeViewHolderRef}
import co.topl.settings.AppContext
import co.topl.nodeView.state.State
import co.topl.wallet.Wallet
import io.circe.parser.parse
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DebugRPCSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // save environment into a variable for reference throughout the application
  protected val appContext = new AppContext(settings, None)

  // Create Bifrost singleton actors
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  // setup route for testing
  val route: Route = DebugApiRoute(settings.restApi, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/debug/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def actOnCurrentView(v: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = v

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CurrentView[History, State, Wallet, MemPool]],
    10.seconds)

  "Debug RPC" should {
    "Get chain information" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "info",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Compute block delay" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "delay",
           |   "params": [{
           |      "blockId": "${view().history.bestBlockId}",
           |      "numBlocks": 1
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Find number of blocks forged by publicKeys held in current node" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "myBlocks",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Return a map of public keys to the number of blocks they have forged in the chain's history" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "generators",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    /*"Print the full chain" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "chain",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
     */

    //Currently not implemented
//    "Check if node is synced to canonical chain" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |   "jsonrpc": "2.0",
//           |   "id": "1",
//           |   "method": "sync",
//           |   "params": [{}]
//           |}
//        """.stripMargin)
//
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//      }
//    }

  }


  object DebugRPCSpec {
    val path: Path = Path("/tmp/bifrost/test-data")
    Try(path.deleteRecursively())
  }
}
