package co.topl.attestation.ops

import co.topl.utils.CommonGenerators
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.attestation.implicits._
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
    }
  }
}
