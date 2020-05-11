package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.api.http.ProgramApiRoute
import modifier.box.{BifrostBox, CodeBoxSerializer}
import io.circe.parser.parse
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58

class ProgramMethodExecutionSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramMockState {

  val route: Route = ProgramApiRoute(settings, nodeViewHolderRef, networkController).route

  "executeProgramMethod" should {

    val boxState: Set[BifrostBox] = Set(stateBox, codeBox, executionBox)

    manuallyApplyBoxes(boxState, 1)

    "Update mutable state in a Program and return the updated state" in {

      val requestBody = ByteString(
        s"""{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "executeProgramMethod",
           |  "params": [{
           |    "owner": "$publicKey",
           |    "signatures": {
           |      "$publicKey": ""
           |    },
           |    "methodName": "add",
           |    "methodParams": {
           |      "x": 2,
           |      "y": 2
           |    },
           |    "programId": "${Base58.encode(executionBox.id)}",
           |    "preFeeBoxes": {
           |      "$publicKey": [[${polyBoxes.head.box.nonce}, ${polyBoxes.head.box.value}]]
           |     },
           |     "fees": {
           |      "$publicKey": 0
           |     },
           |     "timestamp": ${System.currentTimeMillis},
           |     "data": ""
           |  }]
           |}
           |""".stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get

        println(res)

        (res \\ "result").head.isObject shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
