package co.topl.api

import akka.util.ByteString
import co.topl.attestation.Address
import co.topl.http.api.ApiEndpoint
import co.topl.http.api.endpoints.TransactionApiEndpoint
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
  val endpoint: ApiEndpoint = TransactionApiEndpoint(settings.rpcApi, appContext, nodeViewHolderRef)

  val address: Address = keyRing.generateKeyFile("test").get

  var asset: Option[AssetBox] = None
  var tx: Json = "".asJson

  "Asset RPC" should {

    "Create assets prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "topl_rawAssetTransfer",
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

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        tx = ((res \\ "result").head \\ "formattedTx").head
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }
}