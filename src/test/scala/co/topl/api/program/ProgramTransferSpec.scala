package co.topl.api.program

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.http.api.endpoints.ProgramApiRoute
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.DoNotDiscover

import scala.util.Random

@DoNotDiscover
class ProgramTransferSpec extends ProgramRPCMockState {

  val route: Route = ProgramApiRoute(settings.rpcApi, nodeViewHolderRef).route

  "ProgramTransfer" should {

    val boxState = Seq(stateBox, codeBox, executionBox)

    val version = Random.nextInt

    directlyAddPBRStorage(version, boxState)

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
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}

        (res \\ "result").head.asObject.isDefined shouldEqual true
        (res \\ "error").isEmpty shouldEqual true
      }
    }
  }
}
