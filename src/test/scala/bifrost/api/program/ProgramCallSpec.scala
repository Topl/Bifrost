package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import bifrost.api.http.ProgramApiRoute
import bifrost.transaction.box.BifrostBox
import org.scalatest.{Matchers, WordSpec}
import io.circe.syntax._
import scorex.crypto.encode.Base58

class ProgramCallSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramTestState {

  /*val route: Route = ProgramApiRoute(settings, nodeViewHolderRef, networkController).route

  "programCall" should {

    val boxState: Set[BifrostBox] = Set(stateBox, codeBox, executionBox)

    manuallyApplyBoxes(boxState, 1)

    view().vault.boxes().foreach{ b =>
      println(s"${b.box}")
    }

    "Return variable from state of a program" in {

      val programBodyTemplate =
        s"""
      {
        "jsonrpc": "2.0",
        "id": "1",
        "method": "programCall",
        "params": [{
          "programId": "",
          "stateVar": "a",
          "fees": ${fees.asJson},
          "timestamp": ${System.currentTimeMillis},
          "data": ""
        }]
      }
      """
    }
  }*/
}
