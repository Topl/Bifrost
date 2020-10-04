package co.topl.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import co.topl.crypto.{ PrivateKey25519, Signature25519 }
import co.topl.http.api.routes.ProgramApiRoute
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.ArbitBox
import io.circe.parser.parse
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CodeCreationSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramMockState {

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
        val res = parse(responseAs[String]).right.get

        println(s"$res")

        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true

        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txHashId = ModifierId(Base58.decode(txHash).get)
        val txInstance: Transaction = view().pool.getById(txHashId).get

        val history = view().history
        val tempBlock = Block.create(
          history.bestBlockId,
          System.currentTimeMillis(),
          Seq(txInstance),
          ArbitBox(prop, 0L, 10000L),
          gw.secrets.head,
          settings.forgingSettings.version
        )

        view().state.applyModifier(tempBlock)
        view().pool.remove(txInstance)
      }
    }
  }
}
