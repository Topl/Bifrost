package bifrost.api.program

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import bifrost.api.http.ProgramApiRoute
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.encode.Base58

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
        val txInstance: Transaction = view().pool.getById(Base58.decode(txHash).get).get

        /*val history = view().history
        val tempBlock = Block(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
          Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(txInstance),
          10L,
          settings.version
        )
        view().state.applyModifier(tempBlock)
        view().pool.remove(txInstance)

         */
      }
    }
  }
}
