package co.topl.attestation.ops

import co.topl.utils.CommonGenerators
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.attestation.implicits._

class EvidenceOpsSpec
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with CommonGenerators
    with ScalaCheckDrivenPropertyChecks {

  describe("EvidenceOps") {
    describe("toTypedEvidence") {
      it("should convert an Evidence to a TypedEvidence with the same type prefix") {
        forAll(evidenceGen) { evidence =>
          val typedEvidence = evidence.toTypedEvidence

          typedEvidence.value.typePrefix shouldBe evidence.evBytes.head
        }
      }

      it("should convert an Evidence to a TypedEvidence with the same data") {
        forAll(evidenceGen) { evidence =>
          val typedEvidence = evidence.toTypedEvidence

          typedEvidence.value.evidence.data.toArray shouldBe evidence.evBytes.tail
        }
      }
    }
  }
}
