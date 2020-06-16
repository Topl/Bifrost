package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.api.http.ProgramApiRoute
import bifrost.crypto.Signature25519
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

class CodeCreationSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with ProgramMockState {

  val route: Route = ProgramApiRoute(settings, nodeViewHolderRef, networkController).route

  "CodeCreation" should {

    "Create new CodeBox in state" in {

      val requestBody = ByteString(
        s"""{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "createCode",
           |  "params": [{
           |    "publicKey": "$publicKey",
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
        val tempBlock = Block(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, 10000L),
          Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(txInstance),
          10L,
          settings.forgingSettings.version
        )
        view().state.applyModifier(tempBlock)
        view().pool.remove(txInstance)
      }
    }
  }
}
