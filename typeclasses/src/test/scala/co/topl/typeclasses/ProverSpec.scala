package co.topl.typeclasses

import co.topl.models._
import ModelGenerators._
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.typeclasses.implicits._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
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
    forAll { (sk: SecretKeys.Curve25519, data: Bytes) =>
      whenever(data.nonEmpty) {
        val proof = Prover[(SecretKeys.Curve25519, Bytes), Proofs.Knowledge.Curve25519].proveWith((sk, data))

        Curve25519.instance.verify(proof, data, sk.vk[VerificationKeys.Curve25519])
      }
    }
  }

  "ed25519Proves" should "generate a proof" in {
    forAll { (sk: SecretKeys.Ed25519, data: Bytes) =>
      whenever(data.nonEmpty) {
        implicit val ed25519: Ed25519 = new Ed25519
        val proof = Prover[(SecretKeys.Ed25519, Bytes), Proofs.Knowledge.Ed25519].proveWith((sk, data))

        ed25519.verify(proof, data, sk.vk[VerificationKeys.Ed25519])
      }
    }
  }

  "extendedEd25519Proves" should "generate a proof" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, data: Bytes) =>
      whenever(data.nonEmpty) {
        implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519
        val proof = Prover[(SecretKeys.ExtendedEd25519, Bytes), Proofs.Knowledge.Ed25519].proveWith((sk, data))

        extendedEd25519.verify(proof, data, sk.vk[VerificationKeys.ExtendedEd25519])
      }
    }
  }

  "blockProvesHeightLock" should "generate a proof from nothing" in {
    val proof = Prover[Any, Proofs.Contextual.HeightLock].proveWith(())
    assert(true)
  }
}
