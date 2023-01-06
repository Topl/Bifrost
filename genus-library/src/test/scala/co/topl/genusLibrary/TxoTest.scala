package co.topl.genusLibrary

import co.topl.models.Box.Values.AssetV1.Code
import co.topl.models.Box.Values.{Arbit, AssetV1, Empty, Poly}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Base58, Sized}
import co.topl.numerics.implicits._

import scala.util.Random

class TxoTest extends munit.FunSuite {
  private val evidenceLength = 32

  private def randomEvidence =
    TypedEvidence(Random.nextInt().asInstanceOf[Byte], Sized.strictUnsafe(Bytes(Random.nextBytes(evidenceLength))))

  test("empty TxO") {
    val txo = Txo(
      Box(randomEvidence, Empty),
      TxoState.Unspent,
      Box.Id(TypedBytes(Bytes(Random.nextBytes(TypedEvidence.typedEvidenceLength))), 2),
      None
    )

    assert(txo.quantity.isEmpty, "Empty Txo should have no value")
    assertEquals(txo.assetLabel, "EMPTY", "Asset label for empty Txo should be \"Empty\"")
    assert(txo.securityRoot.isEmpty, "Empty Txo should have no securityRoot.")
    assert(txo.metadata.isEmpty, "Empty Txo should have no metadata.")
  }

  test("LVL TxO") {
    val quantity: Int128 = 10_000_000L
    val txo = Txo(
      Box(randomEvidence, Poly(quantity)),
      TxoState.Spent,
      Box.Id(TypedBytes(Bytes(Random.nextBytes(TypedEvidence.typedEvidenceLength))), 2),
      Some(SpendingAddress(randomEvidence))
    )

    assertEquals(quantity, txo.quantity.get, "Txo quantity should match")
    assertEquals(txo.assetLabel, "LVL", "Asset label for LVL Txo should be \"LVL\"")
    assert(txo.securityRoot.isEmpty, "LVL Txo should have no securityRoot.")
    assert(txo.metadata.isEmpty, "LVL Txo should have no metadata.")
  }

  test("TOPL TxO") {
    val quantity: Int128 = 19_000_000L
    val txo = Txo(
      Box(randomEvidence, Arbit(quantity)),
      TxoState.Pending,
      Box.Id(TypedBytes(Bytes(Random.nextBytes(TypedEvidence.typedEvidenceLength))), 2),
      Some(SpendingAddress(randomEvidence))
    )

    assertEquals(quantity, txo.quantity.get, "Txo quantity should match")
    assertEquals(txo.assetLabel, "TOPL", "Asset label for TOPL Txo should be \"LVL\"")
    assert(txo.securityRoot.isEmpty, "TOPL Txo should have no securityRoot.")
    assert(txo.metadata.isEmpty, "TOPL Txo should have no metadata.")
  }

  test("AssetV1 TxO") {
    val quantity: Int128 = 19_099_000L
    val issuingAddress = SpendingAddress(randomEvidence)
    val code = Code(issuingAddress, Sized.maxUnsafe(Latin1Data("foo"))(latin1DataLength, `8`))
    val securityRootLength = 32
    val securityRoot = Sized.strictUnsafe(Bytes(Random.nextBytes(securityRootLength)))(bytesLength, `32`)
    val metadata: Box.Values.AssetV1.Metadata =
      Sized.maxUnsafe(Latin1Data("12345678901234567890123456789012"))(latin1DataLength, `127`)
    val txo = Txo(
      Box(randomEvidence, AssetV1(quantity, code, securityRoot, Some(metadata))),
      TxoState.Unspent,
      Box.Id(TypedBytes(Bytes(Random.nextBytes(TypedEvidence.typedEvidenceLength))), 2),
      Some(SpendingAddress(randomEvidence))
    )

    assertEquals(quantity, txo.quantity.get, "Txo quantity should match")
    val expectedAssetLabel = Base58.encode(issuingAddress.typedEvidence.allBytes.toArray)
    assertEquals(txo.assetLabel, expectedAssetLabel, "Asset label should be as expected.")
    assert(txo.securityRoot.isDefined, "A security root should be present")
    assertEquals(txo.securityRoot.get.toSeq, securityRoot.data.toArray.toSeq, "securityRoot should be as expected")
    assert(txo.metadata.isDefined, "Metadata should be present")
    assertEquals(txo.metadata.get.toSeq, metadata.data.bytes.toSeq, "metadata should be as expected")
  }
}
