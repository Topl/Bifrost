package co.topl.api.program

import akka.util.ByteString
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.DoNotDiscover

@DoNotDiscover
class ProgramCallSpec extends ProgramRPCMockState {

  "programCall" should {

    val boxState = Seq(stateBox, codeBox, executionBox)
    val version = modifierIdGen.sample.get

    directlyAddPBRStorage(version, boxState)

    "Return variable from state of a program" in {

      val programCallTemplate =
        s"""
        {
          "jsonrpc": "2.0",
          "id": "1",
          "method": "programCall",
          "params": [{
            "programId": "${executionBox.value}",
            "stateVar": "a",
            "fees": ${fees.asJson},
            "timestamp": ${System.currentTimeMillis},
            "data": ""
          }]
        }
        """.stripMargin

      val requestBody = ByteString(programCallTemplate.stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }

        (res \\ "result").head.asNumber.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
