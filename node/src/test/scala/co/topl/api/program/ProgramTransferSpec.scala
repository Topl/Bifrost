package co.topl.api.program

import akka.util.ByteString
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.DoNotDiscover

@DoNotDiscover
class ProgramTransferSpec extends ProgramRPCMockState {

  "ProgramTransfer" should {

    val boxState = Seq(stateBox, codeBox, executionBox)

    val version = modifierIdGen.sample.get

    directlyAddPBRStorage(version, boxState)

    "Transfer a program and create a new ExecutionBox with the updated owner" in {

      val requestBody = ByteString(s"""{
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
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        (res \\ "result").head.asObject.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
