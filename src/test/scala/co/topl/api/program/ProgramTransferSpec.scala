package co.topl.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import co.topl.http.api.routes.ProgramApiRoute
import io.circe.parser.parse
import org.scalatest.DoNotDiscover
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

@DoNotDiscover
class ProgramTransferSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramMockState {

  val route: Route = ProgramApiRoute(settings.rpcApi, nodeViewHolderRef).route

  "ProgramTransfer" should {

    val boxState = Seq(stateBox, codeBox, executionBox)

    directlyAddPBRStorage(1, boxState)

    "Transfer a program and create a new ExecutionBox with the updated owner" in {

      val requestBody = ByteString(
        s"""{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "transferProgram",
           |  "params": [{
           |    "from": "$publicKey",
           |    "to": "$publicKey",
           |    "programId": "${executionBox.value}",
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
