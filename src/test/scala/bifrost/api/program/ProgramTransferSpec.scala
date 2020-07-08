package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.api.http.ProgramApiRoute
import bifrost.modifier.box.Box
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58

class ProgramTransferSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramMockState {

  val route: Route = ProgramApiRoute(settings, nodeViewHolderRef, networkController).route

  "ProgramTransfer" should {

    val boxState: Set[Box] = Set(stateBox, codeBox, executionBox)

    manuallyApplyBoxes(boxState, 1)

    "Transfer a program and create a new ExecutionBox with the updated owner" in {

      val requestBody = ByteString(
        s"""{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "transferProgram",
           |  "params": [{
           |    "from": "$publicKey",
           |    "to": "$publicKey",
           |    "programId": "${Base58.encode(executionBox.id)}",
           |    "fee": 0,
           |    "data": ""
           |  }]
           |}
           |""".stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get

        (res \\ "result").head.asObject.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
