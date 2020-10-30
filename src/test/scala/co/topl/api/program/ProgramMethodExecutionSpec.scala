package co.topl.api.program

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.http.api.routes.ProgramApiRoute
import io.circe.parser.parse

class ProgramMethodExecutionSpec extends ProgramRPCMockState {

  val route: Route = ProgramApiRoute(settings.restApi, nodeViewHolderRef).route

  "executeProgramMethod" should {

    val boxState = Seq(stateBox, codeBox, executionBox)

    directlyAddPBRStorage(1, boxState)

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
           |    "programId": "${executionBox.value}",
           |    "preFeeBoxes": {
           |      "$publicKey": [[${polyBoxes.head.nonce}, ${polyBoxes.head.value}]]
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
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}

        (res \\ "result").head.isObject shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
