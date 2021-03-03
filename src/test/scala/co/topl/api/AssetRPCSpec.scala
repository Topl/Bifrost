package co.topl.api

import akka.util.ByteString
import co.topl.attestation.Address
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AssetRPCSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  val address: Address = keyRing.addresses.head
  val recipients: String = assetToSeqGen.sample.get.asJson.toString()

  "Asset RPC" should {

    "Create new assets raw transaction" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "topl_rawAssetTransfer",
           |   "params": [{
           |     "propositionType": "PublicKeyCurve25519",
           |     "recipients":
           |    [
           |  [
           |    "AUBDNmMJkmHtuyGXkWAB7Bg9X8T4CRDNVXmPvVcQWPMk4RdwR883",
           |      {
           |        "quantity" : "5858200457744262097",
           |        "assetCode" : "PPsSca8fTkx4jDLUvxCoaVw3r3ZabTssC2SKFGbCdRYDNJSRm2q6tQCXYkP",
           |        "metadata" : "ApdGzs6uwKAhuKJQswBWoVAFjNA5B8enBKfxVbzlcQ8EnpxicpRcE9B9Bgn2LGv02kYUSA1h1181ZYeECvr",
           |        "type" : "Asset",
           |        "securityRoot" : "11111111111111111111111111111111"
           |      }]],
           |     "sender": ["$address"],
           |     "changeAddress": "$address",
           |     "consolidationAddress": "$address",
           |     "fee": "1",
           |     "minting": true,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }
}