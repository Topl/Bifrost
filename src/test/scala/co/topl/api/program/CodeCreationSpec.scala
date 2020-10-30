package co.topl.api.program

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.http.api.routes.ProgramApiRoute
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.ArbitBox
import io.circe.parser.parse

class CodeCreationSpec extends ProgramRPCMockState {

  val route: Route = ProgramApiRoute(settings.restApi, nodeViewHolderRef).route

  "CodeCreation" should {

    "Create new CodeBox in state" in {

      val requestBody = ByteString(
        s"""{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "createCode",
           |  "params": [{
           |    "publicKey": "${publicKeys("investor")}",
           |    "code": "add = function(a,b) { return a + b }",
           |    "fee": 0,
           |    "data": ""
           |  }]
           |}
           |""".stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}

        println(s"$res")

        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true

        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txHashId = ModifierId(txHash)
        val txInstance: Transaction = view().pool.modifierById(txHashId).get

        val history = view().history
        val tempBlock = Block.create(
          history.bestBlockId,
          System.currentTimeMillis(),
          Seq(txInstance),
          ArbitBox(prop, 0L, 10000L),
          signSk,
          settings.application.version.blockByte
        )

        view().state.applyModifier(tempBlock)
        view().pool.remove(txInstance)
      }
    }
  }
}
