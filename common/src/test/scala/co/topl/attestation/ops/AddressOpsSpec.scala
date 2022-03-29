package co.topl.attestation.ops

import cats.data.{Chain, NonEmptyChain}
import co.topl.attestation.Evidence
import co.topl.attestation.Evidence.EvidenceContent
import co.topl.utils.CommonGenerators
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.attestation.implicits._
import co.topl.attestation.ops.AddressOps.ToDionAddressFailures
import co.topl.attestation.ops.EvidenceOps.ToTypedEvidenceFailures
import co.topl.models.utility.Sized
import org.scalacheck.Gen
import org.scalatest.EitherValues

class AddressOpsSpec
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with CommonGenerators
    with ScalaCheckDrivenPropertyChecks {

  describe("AddressOps") {
    describe("toDionAddress") {
      it("should convert an Address to a Dion Address with the same network prefix") {
        forAll(addressGen) { address =>
          val dionAddress = address.toDionAddress

          dionAddress.value.networkPrefix.value shouldBe address.networkPrefix
        }
      }

      it("should convert an Address to a Dion Address with the same evidence") {
        forAll(addressGen) { address =>
          val dionAddress = address.toDionAddress

          dionAddress.value.typedEvidence.allBytes.toArray shouldBe address.evidence.evBytes
        }
      }

      it("should fail to convert an Address to a Dion Address if the evidence size is too small") {
        forAll(addressGen, Gen.oneOf(1 to 31)) { (address, int) =>
          // because scalacheck likes to throw in zeroes
          val bytesRemoved = if (int < 1) 1 else int

          val evidenceTypePrefix = address.evidence.evBytes.head
          val shortenedContent = address.evidence.evBytes.tail.take(bytesRemoved)

          val shortenedAddress = address.copy(evidence = Evidence(evidenceTypePrefix +: shortenedContent))

          val dionAddress = shortenedAddress.toDionAddress

          dionAddress shouldBe Left(
            ToDionAddressFailures.InvalidEvidence(
              ToTypedEvidenceFailures.InvalidEvidenceSize(Sized.InvalidLength(shortenedContent.length))
            )
          )
        }
      }

      it("should fail to convert an Address to a Dion Address if the evidence size is too large") {
        forAll(addressGen, AddressOpsSpec.bytesGen) { (address, addedBytes) =>
          val stretchedAddress =
            address.copy(evidence = Evidence(address.evidence.evBytes ++ addedBytes.toNonEmptyList.toList))

          val dionAddress = stretchedAddress.toDionAddress

          dionAddress shouldBe Left(
            ToDionAddressFailures.InvalidEvidence(
              ToTypedEvidenceFailures.InvalidEvidenceSize(Sized.InvalidLength(32 + addedBytes.length.toInt))
            )
          )
        }
      }
    }
  }
}

object AddressOpsSpec {

  val bytesGen: Gen[NonEmptyChain[Byte]] =
    Gen
      .zip(
        Gen.posNum[Int].map(_.toByte),
        Gen.listOf(Gen.posNum[Int].map(_.toByte))
      )
      .map(bytes => NonEmptyChain.one(bytes._1).appendChain(Chain.fromSeq(bytes._2)))
}
