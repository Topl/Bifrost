package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.api.http.ProgramApiRoute
import bifrost.transaction.box.BifrostBox
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
           |    "signingPublicKey": "$publicKey",
           |    "methodName": "add",
           |    "parameters": "2, 2",
           |    "programId": "${Base58.encode(executionBox.id)}",
           |    "fee": 0,
           |    "data": ""
           |  }]
           |}
           |""".stripMargin)
    }
  }
}
