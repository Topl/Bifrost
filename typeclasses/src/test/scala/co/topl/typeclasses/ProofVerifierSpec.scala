package co.topl.typeclasses

import cats._
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.immutable.{ListMap, ListSet}

class ProofVerifierSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  type F[A] = Id[A]

  implicit private val ed25519: Ed25519 = new Ed25519()
  implicit private val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  "publicKeyCurve25519Verifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven) =>
      val proof = Curve25519.instance.sign(sk, unprovenTransaction.signableBytes)
      val proposition = sk.vk[VerificationKeys.Curve25519].proposition[Propositions.Knowledge.Curve25519]

      val transaction =
        Transaction(
          ListMap.from(unprovenTransaction.inputs.map(boxRef => boxRef -> (proposition -> proof))),
          unprovenTransaction.feeOutput,
          unprovenTransaction.coinOutputs,
          unprovenTransaction.fee,
          unprovenTransaction.timestamp,
          unprovenTransaction.data,
          unprovenTransaction.minting
        )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      val result =
        proof.satisfies[F](proposition, context)

      val expected =
        Curve25519.instance.verify(proof, unprovenTransaction.signableBytes, sk.vk[VerificationKeys.Curve25519])

      result shouldBe expected
    }
  }

  "publicKeyEd25519Verifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proof = ed25519.sign(sk, unprovenTransaction.signableBytes)
      val proposition = sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519]

      val transaction =
        Transaction(
          ListMap.from(unprovenTransaction.inputs.map(boxRef => boxRef -> (proposition -> proof))),
          unprovenTransaction.feeOutput,
          unprovenTransaction.coinOutputs,
          unprovenTransaction.fee,
          unprovenTransaction.timestamp,
          unprovenTransaction.data,
          unprovenTransaction.minting
        )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      val result =
        proof.satisfies[F](proposition, context)

      val expected =
        ed25519.verify(proof, unprovenTransaction.signableBytes, sk.vk[VerificationKeys.Ed25519])

      result shouldBe expected
    }
  }

  "publicKeyExtendedEd25519Verifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, unprovenTransaction: Transaction.Unproven) =>
      val proof = extendedEd25519.sign(sk, unprovenTransaction.signableBytes)
      val proposition = sk.vk[VerificationKeys.ExtendedEd25519].proposition[Propositions.Knowledge.ExtendedEd25519]

      val transaction =
        Transaction(
          ListMap.from(unprovenTransaction.inputs.map(boxRef => boxRef -> (proposition -> proof))),
          unprovenTransaction.feeOutput,
          unprovenTransaction.coinOutputs,
          unprovenTransaction.fee,
          unprovenTransaction.timestamp,
          unprovenTransaction.data,
          unprovenTransaction.minting
        )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      val result =
        proof.satisfies[F](
          proposition,
          context
        )

      val expected =
        extendedEd25519.verify(proof, unprovenTransaction.signableBytes, sk.vk[VerificationKeys.ExtendedEd25519])

      result shouldBe expected
    }
  }

  "heightLockVerifier" should "verify a proof" in {
    forAll { headerV2: BlockHeaderV2 =>
      val proof = Proofs.Contextual.HeightLock()
      val proposition = Propositions.Contextual.HeightLock(headerV2.height - 1)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(headerV2.height)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = true

      result shouldBe expected
    }
  }

  "heightLockVerifier" should "verify a proof at the same height as the proposition" in {
    forAll { headerV2: BlockHeaderV2 =>
      val proof = Proofs.Contextual.HeightLock()
      val proposition = Propositions.Contextual.HeightLock(headerV2.height)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(headerV2.height)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = true

      result shouldBe expected
    }
  }

  "heightLockVerifier" should "fail to verify a proof if the proof's header comes before the proposition height" in {
    forAll { headerV2: BlockHeaderV2 =>
      val proof = Proofs.Contextual.HeightLock()
      val proposition = Propositions.Contextual.HeightLock(headerV2.height + 1)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(headerV2.height)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = false

      result shouldBe expected
    }
  }

  "andVerifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L
        .proposition[Propositions.Contextual.HeightLock]
        .and(
          sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519]
        )

      val proof = Proofs.Compositional.And(
        Proofs.Contextual.HeightLock(),
        ed25519.sign(sk, unprovenTransaction.signableBytes)
      )

      val transaction =
        Transaction(
          ListMap.from(unprovenTransaction.inputs.map(boxRef => boxRef -> (proposition -> proof))),
          unprovenTransaction.feeOutput,
          unprovenTransaction.coinOutputs,
          unprovenTransaction.fee,
          unprovenTransaction.timestamp,
          unprovenTransaction.data,
          unprovenTransaction.minting
        )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(51L)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = true

      result shouldBe expected
    }
  }

  "andVerifier" should "fail to verify a proof with invalid a or b sub-proofs" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L
        .proposition[Propositions.Contextual.HeightLock]
        .and(
          sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519]
        )

      val proof = Proofs.Compositional.And(
        Proofs.Contextual.HeightLock(),
        Proofs.False
      )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(51L)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = false

      result shouldBe expected
    }
  }

  "orVerifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L
        .proposition[Propositions.Contextual.HeightLock]
        .or(
          sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519]
        )

      val proof = Proofs.Compositional.Or(
        Proofs.Contextual.HeightLock(),
        ed25519.sign(sk, unprovenTransaction.signableBytes)
      )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(51L)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = true

      result shouldBe expected
    }
  }

  "orVerifier" should "verify a proof with only one valid proof" in {
    forAll { (sk: SecretKeys.Ed25519, sk2: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L
        .proposition[Propositions.Contextual.HeightLock]
        .or(
          sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519]
        )

      val proof = Proofs.Compositional.Or(
        Proofs.Contextual.HeightLock(),
        ed25519.sign(sk2, unprovenTransaction.signableBytes)
      )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(51L)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = true

      result shouldBe expected
    }
  }

  "orVerifier" should "fail to verify a proof with invalid a and b sub-proofs" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L
        .proposition[Propositions.Contextual.HeightLock]
        .or(
          sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519]
        )

      val proof = Proofs.Compositional.Or(
        Proofs.Contextual.HeightLock(),
        Proofs.False
      )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(49L)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = false

      result shouldBe expected
    }
  }

  "thresholdVerifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, sk2: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = List(
        50L.proposition[Propositions.Contextual.HeightLock],
        sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519],
        sk2.vk[VerificationKeys.Curve25519].proposition[Propositions.Knowledge.Curve25519]
      ).threshold(2)

      val proof = Proofs.Compositional.Threshold(
        ListSet(
          Proofs.Contextual.HeightLock(),
          ed25519.sign(sk, unprovenTransaction.signableBytes),
          Proofs.False
        )
      )

      val transaction =
        Transaction(
          ListMap.from(unprovenTransaction.inputs.map(boxRef => boxRef -> (proposition -> proof))),
          unprovenTransaction.feeOutput,
          unprovenTransaction.coinOutputs,
          unprovenTransaction.fee,
          unprovenTransaction.timestamp,
          unprovenTransaction.data,
          unprovenTransaction.minting
        )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(51L)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = true

      result shouldBe expected
    }
  }

  "thresholdVerifier" should "fail to verify if the threshold is not met" in {
    forAll { (sk: SecretKeys.Ed25519, sk2: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = List(
        50L.proposition[Propositions.Contextual.HeightLock],
        sk.vk[VerificationKeys.Ed25519].proposition[Propositions.Knowledge.Ed25519],
        sk2.vk[VerificationKeys.Curve25519].proposition[Propositions.Knowledge.Curve25519]
      ).threshold(2)

      val proof = Proofs.Compositional.Threshold(
        ListSet(
          Proofs.Contextual.HeightLock(),
          ed25519.sign(sk, unprovenTransaction.signableBytes),
          Proofs.False
        )
      )

      val transaction =
        Transaction(
          ListMap.from(unprovenTransaction.inputs.map(boxRef => boxRef -> (proposition -> proof))),
          unprovenTransaction.feeOutput,
          unprovenTransaction.coinOutputs,
          unprovenTransaction.fee,
          unprovenTransaction.timestamp,
          unprovenTransaction.data,
          unprovenTransaction.minting
        )

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(49L)

      val result =
        proof.satisfies[F](proposition, context)

      val expected = false

      result shouldBe expected
    }
  }

}
