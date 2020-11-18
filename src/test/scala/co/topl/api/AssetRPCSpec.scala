package co.topl.api

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.http.api.routes.AssetApiRoute
import co.topl.nodeView.state.box.AssetBox
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AssetRPCSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  // setup route for testing
  val route: Route = AssetApiRoute(settings.restApi, nodeViewHolderRef).route

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

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
           |     "issuer": "${publicKeys("hub")}",
           |     "recipient": "${publicKeys("investor")}",
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
//           |    "signingKeys": ["${publicKeys("hub")}"],
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

    "Transfer target asset prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferTargetAssetsPrototype",
           |   "params": [{
           |     "sender": ["${asset.get.proposition.address}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "assetId": "${asset.get.id}",
           |     "amount": 1,
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

//    "Broadcast transferTargetAssetsPrototype" in {
//      val prop = (tx \\ "from").head.asArray.get.head.asArray.get.head.asString.get
//      val secret = view().vault.secretByPublicImage(PublicKey25519Proposition(PublicKey @@ Base58.decode(prop).get)).get
//      val tempTx = tx.as[AssetTransfer] match {case Right(re) => re; case Left(ex) => throw ex}
//      val sig = PrivateKey25519Companion.sign(secret, tempTx.messageToSign)
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

//    "Transfer a target asset" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |   "jsonrpc": "2.0",
//           |   "id": "1",
//           |   "method": "transferTargetAssets",
//           |   "params": [{
//           |     "sender": ["${Base58.encode(asset.get.proposition.pubKeyBytes)}"],
//           |     "recipient": "${publicKeys("producer")}",
//           |     "assetId": "${asset.get.id}",
//           |     "amount": 1,
//           |     "fee": 0,
//           |     "data": ""
//           |   }]
//           |}
//        """.stripMargin)
//
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//      }
//    }

//    "Transfer some assets" in {
//      val requestBody = ByteString(
//        s"""
//           |{
//           |   "jsonrpc": "2.0",
//           |   "id": "1",
//           |   "method": "transferAssets",
//           |   "params": [{
//           |     "issuer": "${publicKeys("hub")}",
//           |      "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
//           |     "recipient": "${publicKeys("producer")}",
//           |     "amount": 1,
//           |     "assetCode": "etherAssets",
//           |     "fee": 0,
//           |     "data": ""
//           |   }]
//           |}
//        """.stripMargin)
//
//      httpPOST(requestBody) ~> route ~> check {
//        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//
//        //Removing transaction from mempool so as not to affect ProgramRPC tests
//        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
//        val txHashId = ModifierId(Base58.decode(txHash).get)
//        val txInstance: Transaction = view().pool.modifierById(txHashId).get
//        view().pool.remove(txInstance)
//      }
//    }

    "Create transfer assets prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferAssetsPrototype",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "amount": 5,
           |     "assetCode": "etherAssets",
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST("/asset/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }
}