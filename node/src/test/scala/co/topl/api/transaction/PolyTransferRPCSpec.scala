package co.topl.api.transaction

import co.topl.attestation.Address

class PolyTransferRPCSpec extends TransferRPCTestMethods {

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

  "PolyTransfer RPC" should {
    "Create, sign and broadcast new poly transfer raw transaction from a Curve25519 address to itself" in {
      val tx = testCreateSignPolyTransfer(addressCurve25519send, addressCurve25519recv, propTypeCurve25519, 3)
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new poly transfer raw transaction from a Curve25519 address to an Ed25519 address" +
    " address" in {
      val tx = testCreateSignPolyTransfer(addressCurve25519send, addressEd25519send, propTypeCurve25519, 3)
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new poly transfer raw transaction from an Ed25519 address to itself" in {
      val tx = testCreateSignPolyTransfer(addressEd25519send, addressEd25519recv, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }

    "Create, sign and broadcast new poly transfer raw transaction from an Ed25519 address to a Curve25519 address" in {
      val tx = testCreateSignPolyTransfer(addressEd25519send, addressCurve25519send, propTypeEd25519, 3)
      testBroadcastTx(tx)
    }
  }
}
