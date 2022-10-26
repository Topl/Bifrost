package co.topl.genusLibrary.orientDb

import co.topl.models._

import scala.util.Random

class GenusGraphMetadataTest extends munit.FunSuite {
  import GenusGraphMetadata._

  val TypedBytesLength = 33

  test("typedBytes Serialization") {
    val byteArray = Random.nextBytes(TypedBytesLength)
    assertEquals(typedBytesTupleToByteArray(byteArrayToTypedBytesTuple(byteArray)).toSeq, byteArray.toSeq,
      "Round trip serialization of TypedBytes")
  }

  test("EligibilityCertificate Serialization") {
    val serializedLength: Int =
      implicitly[Evidence.Length].value +
        implicitly[Eta.Length].value +
        implicitly[Proofs.Knowledge.VrfEd25519.Length].value +
        implicitly[VerificationKeys.VrfEd25519.Length].value
    val byteArray = Random.nextBytes(serializedLength)
    assertEquals(eligibilityCertificateToByteArray(byteArrayToEligibilityCertificate(byteArray)).toSeq, byteArray.toSeq,
    "Round trip serialization of Eligibility Certificate")
  }
}
