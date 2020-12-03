package co.topl.api

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ ByteString, Timeout }
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.AssetBox
import co.topl.nodeView.{CurrentView, NodeViewHolderRef}
import co.topl.settings.{AppContext, StartupOpts}
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AssetRPCSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  // setup route for testing
  val route: Route = AssetApiRoute(settings.rpcApi, nodeViewHolderRef).route

  val address: PublicKey25519Proposition = keyRing.generateKeyFile("test").get

  var asset: Option[AssetBox] = None
  var tx: Json = "".asJson

  "Asset RPC" should {

    "Create assets prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "createAssetsPrototype",
           |   "params": [{
           |     "issuer": "$address",
           |     "recipient": "$address",
           |     "amount": 10,
           |     "assetCode": "etherAssets",
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST("/asset/", requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        tx = ((res \\ "result").head \\ "formattedTx").head
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

//    "Sign createAssets Prototype transaction" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |  "jsonrpc": "2.0",
//           |  "id": "3",
//           |  "method": "signTx",
//           |  "params": [{
//           |    "signingKeys": ["$address"],
//           |    "protoTx": $tx
//           |  }]
//           |}
//          """.stripMargin)
//
//      walletHttpPOST(requestBody) ~> walletRoute ~> check {
//        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
//        tx = (res \\ "result").head
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//      }
//    }
//
//    "Broadcast createAssetsPrototype transaction" in {
//      val secret = view().vault.secretByPublicImage(
//        PublicKey25519Proposition(PublicKey @@ Base58.decode(publicKeys("hub")).get)).get
//      val tempTx = tx.as[AssetCreation] match {case Right(re) => re; case Left(ex) => throw ex}
//      val sig = secret.sign(tempTx.messageToSign)
//      val signedTx = tempTx.copy(signatures = Map(PublicKey25519Proposition(PublicKey @@ Base58.decode(publicKeys("hub")).get) -> sig))
//
//      val requestBody = ByteString(
//        s"""
//           |{
//           |  "jsonrpc": "2.0",
//           |  "id": "1",
//           |  "method": "broadcastTx",
//           |  "params": [{
//           |    "tx": ${signedTx.json}
//           |  }]
//           |}
//        """.stripMargin)
//
//      walletHttpPOST(requestBody) ~> walletRoute ~> check {
//        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//      }
//    }
//
//    "Create transfer assets prototype" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |   "jsonrpc": "2.0",
//           |   "id": "1",
//           |   "method": "transferAssetsPrototype",
//           |   "params": [{
//           |     "issuer": "$address,
//           |     "sender": ["$address"],
//           |     "recipient": "$address",
//           |     "amount": 5,
//           |     "assetCode": "etherAssets",
//           |     "fee": 0,
//           |     "data": ""
//           |   }]
//           |}
//        """.stripMargin)
//
//      httpPOST("/asset/", requestBody) ~> route ~> check {
//        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//      }
//    }
  }
}