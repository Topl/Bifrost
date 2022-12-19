package co.topl.typeclasses

import co.topl.models._

import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import co.topl.ModelGenerators._

class ProposerSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  "curve25519Proposes" should "propose" in {
    forAll { vk: VerificationKeys.Curve25519 =>
      Proposer.instances.curve25519Proposes.propositionOf(vk) shouldBe Propositions.Knowledge.Curve25519(vk)
    }
  }

  "ed25519Proposes" should "propose" in {
    forAll { vk: VerificationKeys.Ed25519 =>
      Proposer.instances.ed25519Proposes.propositionOf(vk) shouldBe Propositions.Knowledge.Ed25519(vk)
    }
  }

  "extendedEd25519Proposes" should "propose" in {
    forAll { vk: VerificationKeys.ExtendedEd25519 =>
      Proposer.instances.extendedEd25519Proposes.propositionOf(vk) shouldBe Propositions.Knowledge.ExtendedEd25519(vk)
    }
  }

  "longProposesHeightLock" should "propose" in {
    forAll { height: Long =>
      Proposer.instances.longProposesHeightLock.propositionOf(height) shouldBe Propositions.Contextual.HeightLock(
        height
      )
    }
  }

}
