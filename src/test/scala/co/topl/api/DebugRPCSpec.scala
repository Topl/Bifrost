package co.topl.api

import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.ByteString
import co.topl.http.api.routes.DebugApiRoute
import co.topl.nodeView.CurrentView
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import io.circe.parser.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class DebugRPCSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  // setup route for testing
  val route: Route = DebugApiRoute(settings.restApi, nodeViewHolderRef).route

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView).mapTo[CurrentView[History, State, MemPool]],
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

      httpPOST("/debug/", requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
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

      httpPOST("/debug/", requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
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

      httpPOST("/debug/", requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
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

      httpPOST("/debug/", requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

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
//        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//      }
//    }

  }
}
