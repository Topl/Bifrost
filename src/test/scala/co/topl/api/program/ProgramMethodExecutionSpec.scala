package co.topl.api.program

import akka.util.ByteString
import io.circe.parser.parse
import org.scalatest.DoNotDiscover

@DoNotDiscover
class ProgramMethodExecutionSpec extends ProgramRPCMockState {

  "executeProgramMethod" should {

    val boxState = Seq(stateBox, codeBox, executionBox)
    val version = modifierIdGen.sample.get

    directlyAddPBRStorage(version, boxState)

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
           |    "fees": {
           |      "$publicKey": 0
           |    },
           |    "timestamp": ${System.currentTimeMillis},
           |    "data": ""
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
