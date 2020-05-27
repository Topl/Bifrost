package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.BifrostGenerators
import bifrost.api.http.DebugApiRoute
import bifrost.history.History
import bifrost.mempool.MemPool
import bifrost.nodeView.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.nodeView.NodeViewHolder
import bifrost.state.State
import bifrost.wallet.Wallet
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

class DebugRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new NodeViewHolder(settings)))
  val route = DebugApiRoute(settings, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/debug/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  implicit val timeout = Timeout(10.seconds)

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[History, State, Wallet, MemPool]], 10.seconds)

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
           |      "blockId": "${Base58.encode(view.history.bestBlockId)}",
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
