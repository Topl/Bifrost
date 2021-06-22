package co.topl.api.transaction

import co.topl.attestation.Address
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.codecs.implicits.base58JsonDecoder
import co.topl.utils.encode.Base58
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArbitTransferRPCSpec extends TransferRPCTestMethods {

  var addressCurve25519send: Address = _
  var addressCurve25519recv: Address = _
  var addressEd25519send: Address = _
  var addressEd25519recv: Address = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    addressCurve25519send = keyRingCurve25519.addresses.head
    addressCurve25519recv = keyRingCurve25519.addresses.tail.head
    addressEd25519send = keyRingEd25519.addresses.head
    addressEd25519recv = keyRingEd25519.addresses.tail.head
  }

  "ArbitTransfer RPC" should {
    "Create, sign and broadcast new arbit transfer raw transaction from a Curve25519 address to itself" in {
      val tx = testCreateSignArbitTransfer(addressCurve25519send, addressCurve25519recv, propTypeCurve25519, 3)
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new arbit transfer raw transaction from a Curve25519 address to an Ed25519 address" +
    " address" in {
      val tx = testCreateSignArbitTransfer(addressCurve25519send, addressEd25519send, propTypeCurve25519, 3)
      testBroadcastTx(tx)
    }

        val sigTx = for {
          rawTx   <- res.hcursor.downField("result").get[Json]("rawTx")
          message <- res.hcursor.downField("result").get[Base58Data]("messageToSign")
        } yield {
          val sig = keyRing.generateAttestation(address)(message.value)
          val signatures: Json = Map(
            "signatures" -> sig.asJson
          ).asJson
          rawTx.deepMerge(signatures)
        }

        tx = sigTx.value.toString

        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Create, sign and broadcast new arbit transfer raw transaction from an Ed25519 address to a Curve25519 address" in {
      val tx = testCreateSignArbitTransfer(addressEd25519send, addressCurve25519send, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }
  }
}
