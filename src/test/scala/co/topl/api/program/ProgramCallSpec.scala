package co.topl.api.program

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.http.api.routes.ProgramApiRoute
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.DoNotDiscover

import scala.util.Random

@DoNotDiscover
class ProgramCallSpec extends ProgramRPCMockState {

  val route: Route = ProgramApiRoute(settings.restApi, nodeViewHolderRef).route

  "programCall" should {

    val boxState = Seq(stateBox, codeBox, executionBox)
    val version = Random.nextInt

    directlyAddPBRStorage(version, boxState)

    view().history.bestBlock.transactions.foreach{ tx =>
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
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}

        (res \\ "result").head.asNumber.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
