package co.topl.api.transaction

import co.topl.attestation.Address
import co.topl.modifier.box.AssetCode
import co.topl.utils.GeneratorOps.GeneratorOps
import co.topl.utils.StringDataTypes.Latin1Data
import io.circe.Json
import io.circe.syntax._

class AssetTransferRPCSpec extends TransferRPCTestMethods {

  var addressCurve25519Fst: Address = _
  var addressCurve25519Sec: Address = _
  var addressEd25519Fst: Address = _
  var addressEd25519Sec: Address = _
  var addressThresholdCurve25519Fst: Address = _
  var addressThresholdCurve25519Sec: Address = _
  var recipients: String = _
  var assetCodeCurve25519Fst: AssetCode = _
  var assetCodeEd25519Fst: AssetCode = _
  var assetCodeThresholdCurve25519Fst: AssetCode = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    addressCurve25519Fst = keyRingCurve25519.addresses.head
    addressCurve25519Sec = keyRingCurve25519.addresses.last
    addressEd25519Fst = keyRingEd25519.addresses.head
    addressEd25519Sec = keyRingEd25519.addresses.last
    addressThresholdCurve25519Fst = propsThresholdCurve25519.head.address
    addressThresholdCurve25519Sec = propsThresholdCurve25519.last.address
    recipients = assetToSeqGen.sample.get.asJson.toString()
    assetCodeCurve25519Fst = AssetCode(1: Byte, addressCurve25519Fst, Latin1Data.unsafe("test"))
    assetCodeEd25519Fst = AssetCode(1: Byte, addressEd25519Fst, Latin1Data.unsafe("test"))
    assetCodeThresholdCurve25519Fst = AssetCode(1: Byte, addressThresholdCurve25519Fst, Latin1Data.unsafe("test"))
  }

  "AssetTransfer RPC" should {
    "Create, sign and broadcast new asset transfer raw transaction from a Curve25519 address to itself" in {
      val tx =
        testCreateSignAssetTransfer(
          addressCurve25519Fst,
          addressCurve25519Sec,
          assetCodeCurve25519Fst,
          propTypeCurve25519,
          3
        )
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer from a Curve25519 address to an Ed25519 address" +
    " address" in {
      val tx =
        testCreateSignAssetTransfer(
          addressCurve25519Fst,
          addressEd25519Fst,
          assetCodeCurve25519Fst,
          propTypeCurve25519,
          3
        )
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer from an Ed25519 address to itself" in {
      val tx =
        testCreateSignAssetTransfer(addressEd25519Fst, addressEd25519Sec, assetCodeEd25519Fst, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer from an Ed25519 address to a Curve25519 address" in {
      val tx =
        testCreateSignAssetTransfer(addressEd25519Fst, addressCurve25519Fst, assetCodeEd25519Fst, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer from a threshold Curve25519 address to an address of same type" in {
      val tx =
        testCreateSignAssetTransfer(
          addressThresholdCurve25519Fst,
          addressThresholdCurve25519Sec,
          assetCodeThresholdCurve25519Fst,
          propTypeThresholdCurve25519,
          3
        )
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer from a threshold Curve25519 address to a Ed25519 address" in {
      val tx =
        testCreateSignAssetTransfer(
          addressThresholdCurve25519Fst,
          addressEd25519Fst,
          assetCodeThresholdCurve25519Fst,
          propTypeThresholdCurve25519,
          3
        )
      testBroadcastTx(tx)
    }

    "Threshold transaction with invalid attestation type should error" in {
      val tx = testCreateSignAssetTransfer(
        addressThresholdCurve25519Fst,
        addressCurve25519Sec,
        assetCodeThresholdCurve25519Fst,
        propTypeThresholdCurve25519,
        3
      )
      val attestation: Json = Map("signatures" -> attestationCurve25519Gen.sample.get.asJson).asJson
      testBroadcastInvalidTx(tx.hcursor.downField("signatures").delete.top.get.deepMerge(attestation))
    }
  }
}
