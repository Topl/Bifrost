package co.topl.api.transaction

import co.topl.attestation.Address
import co.topl.modifier.box.AssetCode
import io.circe.syntax._

class AssetTransferRPCSpec extends TransferRPCTestMethods {

  var addressCurve25519send: Address = _
  var addressCurve25519recv: Address = _
  var addressEd25519send: Address = _
  var addressEd25519recv: Address = _
  var recipients: String = _
  var assetCodeCurve25519: AssetCode = _
  var assetCodeEd25519: AssetCode = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    addressCurve25519send = keyRingCurve25519.addresses.head
    addressCurve25519recv = keyRingCurve25519.addresses.tail.head
    addressEd25519send = keyRingEd25519.addresses.head
    addressEd25519recv = keyRingEd25519.addresses.tail.head
    recipients = assetToSeqGen.sample.get.asJson.toString()
    assetCodeCurve25519 = AssetCode(1: Byte, addressCurve25519send, "test")
    assetCodeEd25519 = AssetCode(1: Byte, addressEd25519send, "test")
  }

  "AssetTransfer RPC" should {
    "Create, sign and broadcast new asset transfer raw transaction from a Curve25519 address to itself" in {
      val tx = testCreateSignAssetTransfer(
        addressCurve25519send,
        addressCurve25519recv,
        assetCodeCurve25519,
        propTypeCurve25519,
        3
      )
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer raw transaction from a Curve25519 address to an Ed25519 address" +
    " address" in {
      val tx = testCreateSignAssetTransfer(
        addressCurve25519send,
        addressEd25519send,
        assetCodeCurve25519,
        propTypeCurve25519,
        3
      )
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer raw transaction from an Ed25519 address to itself" in {
      val tx = testCreateSignAssetTransfer(addressEd25519send, addressEd25519recv, assetCodeEd25519, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new asset transfer raw transaction from an Ed25519 address to a Curve25519 address" in {
      val tx =
        testCreateSignAssetTransfer(addressEd25519send, addressCurve25519send, assetCodeEd25519, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }
  }
}
