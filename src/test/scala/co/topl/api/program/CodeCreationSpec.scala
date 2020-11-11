package co.topl.api.program

import akka.http.scaladsl.server.Route
import co.topl.http.api.routes.ProgramApiRoute
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.ArbitBox
import co.topl.utils.RPCHelpers
import io.circe.parser.parse

class CodeCreationSpec extends ProgramRPCMockState with RPCHelpers {

  val route: Route = ProgramApiRoute(settings.restApi, nodeViewHolderRef).route

  "CodeCreation" should {

    "Create new CodeBox in state" in {

      println(s"$publicKey")
      println(s"${publicKey.pubKeyBytes.length}")

      val params =
        s"""
           |"publicKey": "$publicKey",
           |"code": "add = function(a,b) { return a + b }",
           |"fee": 0,
           |"data": ""
           |""".stripMargin

      val request = formRequest("createCode", params)

      httpPOST(request) ~> route ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}

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
          ArbitBox(publicKey, 0L, 10000L),
          signSk,
          settings.application.version.blockByte
        )

        view().state.applyModifier(tempBlock)
        view().pool.remove(txInstance)
      }
    }
  }
}
