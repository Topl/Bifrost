package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.api.http.ProgramApiRoute
import bifrost.transaction.box.BifrostBox
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import io.circe.syntax._
import scorex.crypto.encode.Base58

class ProgramCallSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramMockState {

  val route: Route = ProgramApiRoute(settings, nodeViewHolderRef, networkController).route

  "programCall" should {

    val boxState: Set[BifrostBox] = Set(stateBox, codeBox, executionBox)

    manuallyApplyBoxes(boxState, 1)

    view().history.bestBlock.txs.foreach{tx =>
      println(s"${tx.toString}")
    }

    "Return variable from state of a program" in {

      val programCallTemplate =
        s"""
        {
          "jsonrpc": "2.0",
          "id": "1",
          "method": "programCall",
          "params": [{
            "programId": "${Base58.encode(executionBox.id)}",
            "stateVar": "a",
            "fees": ${fees.asJson},
            "timestamp": ${System.currentTimeMillis},
            "data": ""
          }]
        }
        """.stripMargin

      val requestBody = ByteString(programCallTemplate.stripMargin)
      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get

        println(s"res: $res")

        (res \\ "result").head.asNumber.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
