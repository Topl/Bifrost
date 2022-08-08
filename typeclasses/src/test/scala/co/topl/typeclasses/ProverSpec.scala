package co.topl.typeclasses

import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models.ModelGenerators._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ProverSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  "curve25519Proves" should "generate a proof" in {
    forAll { (sk: SecretKeys.Curve25519, data: Transaction.Unproven) =>
      val proof = (sk, data).asProof.asInstanceOf[Proofs.Knowledge.Curve25519]

      Curve25519.instance.verify(proof, data.signableBytes, sk.vk.asInstanceOf[VerificationKeys.Curve25519])
    }
  }

  "ed25519Proves" should "generate a proof" in {
    forAll { (sk: SecretKeys.Ed25519, data: Transaction.Unproven) =>
      implicit val ed25519: Ed25519 = new Ed25519
      val proof = (sk, data).asProof.asInstanceOf[Proofs.Knowledge.Ed25519]

      ed25519.verify(proof, data.signableBytes, sk.vk.asInstanceOf[VerificationKeys.Ed25519])
    }
  }

  "extendedEd25519Proves" should "generate a proof" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, data: Transaction.Unproven) =>
      implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519
      val proof = (sk, data).asProof.asInstanceOf[Proofs.Knowledge.Ed25519]

      extendedEd25519.verify(proof, data.signableBytes, sk.vk.asInstanceOf[VerificationKeys.ExtendedEd25519])
    }
  }
}
