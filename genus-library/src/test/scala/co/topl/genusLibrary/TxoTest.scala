package co.topl.genusLibrary

import co.topl.models.Box.Values.{Empty, Poly}
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models._
import co.topl.typeclasses.implicits._

import scala.util.Random

class TxoTest extends munit.FunSuite {
  private val evidenceLength = 32

  private def randomEvidence =
    TypedEvidence(Random.nextInt.asInstanceOf[Byte], Sized.strictUnsafe(Bytes(Random.nextBytes(evidenceLength))))

  test("empty TxO") {
    val txo = Txo(
      Box(randomEvidence, Empty),
      TxoState.Unspent,
      Box.Id(TypedBytes(Bytes(Random.nextBytes(evidenceLength + 1))), 2),
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
      TxoState.Unspent,
      Box.Id(TypedBytes(Bytes(Random.nextBytes(evidenceLength + 1))), 2),
      Some(SpendingAddress(randomEvidence))
    )

    assertEquals(quantity, txo.quantity.get, "Txo quantity should match")
    assertEquals(txo.assetLabel, "LVL", "Asset label for LVL Txo should be \"LVL\"")
    assert(txo.securityRoot.isEmpty, "LVL Txo should have no securityRoot.")
    assert(txo.metadata.isEmpty, "LVL Txo should have no metadata.")
  }
}
