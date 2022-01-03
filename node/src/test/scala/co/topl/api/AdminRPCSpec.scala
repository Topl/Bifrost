package co.topl.api

import akka.util.ByteString
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AdminRPCSpec extends AnyWordSpec with Matchers with RPCMockState with EitherValues {

  "Admin RPC" should {
    "Return informative error when the keyfile being unlocked is not found on disk" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "admin_unlockKeyfile",
        |   "params": [{
        |     "address": "${keyRingCurve25519.addresses.head.toString}",
        |     "password": "test"
        |   }]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val error = res.hcursor.downField("error").as[Json].toString
        error should include("Cannot find a unique matching keyfile")
      }
    }

    "Return a confirmation after successfully updating the reward address" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "admin_updateRewardsAddress",
        |   "params": [{
        |     "address": "${keyRingCurve25519.addresses.last.toString}"
        |   }]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val msg = res.hcursor.downField("result").as[Json].value.toString
        msg should include("Updated reward address")
      }
    }
  }
}
