package co.topl.genusLibrary.orientDb

import cats.data.Chain
import co.topl.brambl.models.Evidence
import co.topl.brambl.models.Identifier.IoTransaction32
import co.topl.crypto.hash.Blake2b256
import co.topl.{models => legacyModels}
import legacyModels.utility._
import co.topl.consensus.models._
import co.topl.crypto.models._
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString
import quivr.models.Digest.Digest32
import scala.util.Random

class GenusGraphMetadataTest extends munit.FunSuite {
  import GenusGraphMetadata._

  private val evidenceLength: Length = implicitly[legacyModels.Evidence.Length]
  private val TypedBytesLength = 33
  private val byteStringLength32 = ByteString.copyFrom(Array.fill(Lengths.`32`.value)(0: Byte))

  test("typedBytes Serialization") {
    val byteArray = Random.nextBytes(TypedBytesLength)
    assertEquals(
      typedBytesTupleToByteArray(byteArrayToTypedBytesTuple(byteArray)).toSeq,
      byteArray.toSeq,
      "Round trip serialization of TypedBytes"
    )
  }

  test("EligibilityCertificate Serialization") {
    val eligibilityCertificate = EligibilityCertificate(
      vrfSig = SignatureVrfEd25519.of(ByteString.copyFrom(Array.fill(Lengths.`80`.value)(0: Byte))),
      vrfVK = VerificationKeyVrfEd25519.of(byteStringLength32),
      thresholdEvidence = ByteString.copyFrom(Array.fill[Byte](evidenceLength.value)(0)),
      eta = ByteString.copyFrom(new Blake2b256().hash(legacyModels.Bytes(Random.nextBytes(TypedBytesLength))).toArray)
    )

    assertEquals(
      byteArrayToEligibilityCertificate(eligibilityCertificateToByteArray(eligibilityCertificate)),
      eligibilityCertificate,
      "Round trip serialization of Eligibility Certificate"
    )
  }

  test("OperationalCertificate Serialization") {
    val operationalCertificate = OperationalCertificate(
      VerificationKeyKesProduct.of(byteStringLength32, step = 0),
      SignatureKesProduct(
        SignatureKesSum(
          VerificationKeyEd25519.of(ByteString.EMPTY),
          SignatureEd25519(ByteString.EMPTY),
          witness = Seq.empty
        ),
        SignatureKesSum(
          VerificationKeyEd25519.of(ByteString.EMPTY),
          SignatureEd25519.of(ByteString.EMPTY),
          witness = Seq.empty
        ),
        subRoot = byteStringLength32
      ),
      VerificationKeyEd25519.of(ByteString.EMPTY),
      SignatureEd25519(ByteString.EMPTY)
    )

    assertEquals(
      byteArrayToOperationalCertificate(operationalCertificateToByteArray(operationalCertificate)),
      operationalCertificate,
      "Round trip serialization of OperationalCertificate"
    )
  }

  test("BlockBody round-trip Serialization") {
    val transactions = (0 to 3).foldLeft(Seq.empty[IoTransaction32]) { case (transactions, _) =>
      val byteArray = Random.nextBytes(evidenceLength.value)
      val transactionId =
        IoTransaction32.of(Evidence.Sized32.of(Digest32.of(ByteString.copyFrom(byteArray))))

      transactions :+ transactionId
    }
    val blockBody = BlockBody.of(transactions)

    assertEquals(
      byteArrayToBlockBody(blockBodyToByteArray(blockBody)),
      blockBody,
      "Round trip serialization of BlockBody"
    )
  }

  test("Transaction round-trip serialization") {
    // noinspection ScalaStyle
    val transaction = legacyModels.Transaction(
      inputs = Chain.empty,
      outputs = Chain.empty,
      schedule = legacyModels.Transaction.Schedule(0L, 100L, 1000L),
      data = None
    )
    assertEquals(
      byteArrayToTransaction(transactionToByteArray(transaction)),
      transaction,
      "Round-Trip Transaction serialization"
    )
  }
}
