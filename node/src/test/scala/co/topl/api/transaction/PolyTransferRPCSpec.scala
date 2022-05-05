package co.topl.api.transaction

import co.topl.attestation.Address
import co.topl.utils.DiskKeyRingTestHelper
import io.circe.Json
import io.circe.syntax.EncoderOps

class PolyTransferRPCSpec extends TransferRPCTestMethods {

  var addressCurve25519Fst: Address = _
  var addressCurve25519Sec: Address = _
  var addressEd25519Fst: Address = _
  var addressEd25519Sec: Address = _
  var addressThresholdCurve25519Fst: Address = _
  var addressThresholdCurve25519Sec: Address = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    addressCurve25519Fst = keyRingCurve25519.addresses.head
    addressCurve25519Sec = keyRingCurve25519.addresses.last
    addressEd25519Fst = keyRingEd25519.addresses.head
    addressEd25519Sec = keyRingEd25519.addresses.last
    addressThresholdCurve25519Fst = propsThresholdCurve25519.head.address
    addressThresholdCurve25519Sec = propsThresholdCurve25519.last.address
  }

  "PolyTransfer RPC" should {
    "Create, encode, sign new raw poly transfer from a Curve25519 address to itself, and broadcast it" in {
      val tx = testCreateSignPolyTransfer(addressCurve25519Fst, addressCurve25519Sec, propTypeCurve25519, 3)
      testBroadcastTx(tx)
    }

    "Create, encode, sign new raw poly transfer from a Curve25519 address to an Ed25519 address, and broadcast it" +
    " address" in {
      val tx = testCreateSignPolyTransfer(addressCurve25519Fst, addressEd25519Fst, propTypeCurve25519, 3)
      testBroadcastTx(tx)
    }

    "Create, encode, sign new raw poly transfer from an Ed25519 address to itself, and broadcast it" in {
      val tx = testCreateSignPolyTransfer(addressEd25519Fst, addressEd25519Sec, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }

    "Create, encode, sign new raw poly transfer from an Ed25519 address to a Curve25519 address, and broadcast it" in {
      val tx = testCreateSignPolyTransfer(addressEd25519Fst, addressCurve25519Fst, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }

    "Create, encode, sign new raw poly transfer from a threshold Curve25519 address to an address of same type, and " +
    "broadcast it" in {
      val tx =
        testCreateSignPolyTransfer(
          addressThresholdCurve25519Fst,
          addressThresholdCurve25519Sec,
          propTypeThresholdCurve25519,
          3
        )
      testBroadcastTx(tx)
    }

    "Create, encode, sign new raw poly transfer from a threshold Curve25519 address to a Ed25519 address, and " +
    "broadcast it" in {
      val tx =
        testCreateSignPolyTransfer(
          addressThresholdCurve25519Fst,
          addressEd25519Fst,
          propTypeThresholdCurve25519,
          3
        )
      testBroadcastTx(tx)
    }

    "Threshold transaction with invalid attestation type should error" in {
      val tx = testCreateSignPolyTransfer(
        addressThresholdCurve25519Fst,
        addressCurve25519Sec,
        propTypeThresholdCurve25519,
        3
      )
      val attestation: Json = Map("signatures" -> attestationCurve25519Gen.sample.get.asJson).asJson
      testBroadcastTxInvalidProp(tx.hcursor.downField("signatures").delete.top.get.deepMerge(attestation))
    }
  }
}
