package co.topl.genusLibrary.orientDb

import cats.data.Chain
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility._
import co.topl.models.utility.HasLength.instances.bytesLength

import scala.collection.immutable.ListSet
import scala.util.Random

class GenusGraphMetadataTest extends munit.FunSuite {
  import GenusGraphMetadata._

  private val TypedBytesLength = 33

  private val operationalCertificate: OperationalCertificate = OperationalCertificate(
    VerificationKeys.KesProduct(zeroBytes(Lengths.`32`), 0),
    Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
        Vector.empty
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
        Vector.empty
      ),
      zeroBytes(Lengths.`32`)
    ),
    VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
    Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`))
  )

  def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](Bytes(Array.fill(l.value)(0: Byte)))

  private val eta: Eta = new Blake2b256().hash(Bytes(Random.nextBytes(TypedBytesLength)))

  private val eligibilityCertificate = EligibilityCertificate(
    Proofs.Knowledge.VrfEd25519(zeroBytes(Lengths.`80`)),
    VerificationKeys.VrfEd25519(VerificationKeys.Ed25519(zeroBytes[VerificationKeys.VrfEd25519.Length]).bytes),
    thresholdEvidence = Sized.strictUnsafe(Bytes(Array.fill[Byte](evidenceLength.value)(0))),
    eta = eta
  )

  private val stakingAddressOperator: StakingAddresses.Operator =
    StakingAddresses.Operator(VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)))

  test("typedBytes Serialization") {
    val byteArray = Random.nextBytes(TypedBytesLength)
    assertEquals(
      typedBytesTupleToByteArray(byteArrayToTypedBytesTuple(byteArray)).toSeq,
      byteArray.toSeq,
      "Round trip serialization of TypedBytes"
    )
  }

  test("EligibilityCertificate Serialization") {
    assertEquals(
      byteArrayToEligibilityCertificate(eligibilityCertificateToByteArray(eligibilityCertificate)),
      eligibilityCertificate,
      "Round trip serialization of Eligibility Certificate"
    )
  }

  test("OperationalCertificate Serialization") {
    assertEquals(
      byteArrayToOperationalCertificate(operationalCertificateToByteArray(operationalCertificate)),
      operationalCertificate,
      "Round trip serialization of OperationalCertificate"
    )
  }

  test("StakingAddress Operator Serialization") {
    assertEquals(
      byteArrayToStakingAddressOperator(stakingAddressOperatorToByteArray(stakingAddressOperator)),
      stakingAddressOperator,
      "Round trip serialization of StakingAddress operator"
    )
  }

  test("BlockBody round-trip Serialization") {
    val blockBody = (0 to 3).foldLeft(ListSet.empty[TypedIdentifier]) { case (transactions, _) =>
      val byteArray = Random.nextBytes(evidenceLength.value)
      val transactionId = TypedBytes(IdentifierTypes.Block.BodyV2, Bytes(byteArray))
      transactions + transactionId
    }

    assertEquals(
      byteArrayToBlockBody(blockBodyToByteArray(blockBody)).toSeq,
      blockBody.toSeq,
      "Round trip serialization of BlockBody"
    )
  }

  test("Transaction round-trip serialization") {
    // noinspection ScalaStyle
    val transaction = Transaction(
      inputs = Chain.empty,
      outputs = Chain.empty,
      schedule = Transaction.Schedule(0L, 100L, 1000L),
      data = None
    )
    assertEquals(
      byteArrayToTransaction(transactionToByteArray(transaction)),
      transaction,
      "Round-Trip Transaction serialization"
    )
  }
}
