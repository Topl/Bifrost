package co.topl.typeclasses

import cats._
import cats.implicits._
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import io.circe.Json
import io.circe.syntax._
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.immutable.ListMap

class ProofVerifierSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  import ProofVerifierSpec._

  "publicKeyCurve25519Verifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven) =>
      val proof = Curve25519.instance.sign(sk, unprovenTransaction.signableBytes)
      val proposition = sk.vk.asProposition

      val transaction = unprovenTransaction.prove(_ => proof)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      val result =
        proposition isSatisfiedBy proof

      val expected =
        Curve25519.instance.verify(
          proof,
          unprovenTransaction.signableBytes,
          sk.vk.asInstanceOf[VerificationKeys.Curve25519]
        )

      result shouldBe expected
    }
  }

  "publicKeyEd25519Verifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proof = ed25519.sign(sk, unprovenTransaction.signableBytes)
      val proposition = sk.vk.asProposition

      val transaction = unprovenTransaction.prove(_ => proof)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      val result =
        proof.satisfies[F](proposition)

      val expected =
        ed25519.verify(proof, unprovenTransaction.signableBytes, sk.vk.asInstanceOf[VerificationKeys.Ed25519])

      result shouldBe expected
    }
  }

  "publicKeyExtendedEd25519Verifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, unprovenTransaction: Transaction.Unproven) =>
      val proof = extendedEd25519.sign(sk, unprovenTransaction.signableBytes)
      val proposition = sk.vk.asProposition

      val transaction = unprovenTransaction.prove(_ => proof)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      val result = proposition.isSatisfiedBy(proof)

      val expected =
        extendedEd25519.verify(
          proof,
          unprovenTransaction.signableBytes,
          sk.vk.asInstanceOf[VerificationKeys.ExtendedEd25519]
        )

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
        proof.satisfies[F](proposition)

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
        proof.satisfies[F](proposition)

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
        proof.satisfies[F](proposition)

      val expected = false

      result shouldBe expected
    }
  }

  ignore should "verify a JS proof" in {
    val proposition = Propositions.Script.JS(
      Propositions.Script.JS.JSScript(
        """(ctx, args) =>
          |  ctx.currentHeight > 30 && args["foo"] == 37
          |""".stripMargin
      )
    )

    val proof = Proofs.Script.JS(
      Json.obj("foo" -> 37.asJson).toString
    )

    implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

    val unprovenTransaction = ModelGenerators.arbitraryUnprovenTransaction.arbitrary.first

    val transaction = unprovenTransaction.prove(_ => proof)

    (() => context.currentTransaction)
      .expects()
      .once()
      .returning(transaction)

    (() => context.currentHeight)
      .expects()
      .once()
      .returning(40L)

    (() => context.currentSlot)
      .expects()
      .once()
      .returning(500L)

    val result =
      proof.satisfies[F](proposition)

    val expected = true

    result shouldBe expected
  }

  "andVerifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val heightLockProposition =
        50L.asProposition

      val proposition: Propositions.Compositional.And =
        heightLockProposition
          .and(
            sk.vk.asProposition
          )

      val proof = Proofs.Compositional.And(
        Proofs.Contextual.HeightLock(),
        ed25519.sign(sk, unprovenTransaction.signableBytes)
      )

      val transaction = unprovenTransaction.prove(_ => proof)

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
        proof.satisfies[F](proposition)

      val expected = true

      result shouldBe expected
    }
  }

  "andVerifier" should "fail to verify a proof with invalid a or b sub-proofs" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L.asProposition
        .and(
          sk.vk.asProposition
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
        proof.satisfies[F](proposition)

      val expected = false

      result shouldBe expected
    }
  }

  "orVerifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition =
        50L.asProposition
          .or(
            sk.vk.asProposition
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
        proof.satisfies[F](proposition)

      val expected = true

      result shouldBe expected
    }
  }

  "orVerifier" should "verify a proof with only one valid proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L.asProposition
        .or(
          sk.vk.asProposition
        )

      val proof = Proofs.Compositional.Or(
        Proofs.Contextual.HeightLock(),
        ed25519.sign(sk, unprovenTransaction.signableBytes)
      )

      val transaction = unprovenTransaction.prove(_ => proof)

      implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

      (() => context.currentTransaction)
        .expects()
        .once()
        .returning(transaction)

      (() => context.currentHeight)
        .expects()
        .once()
        .returning(5L)

      val result =
        proof.satisfies[F](proposition)

      val expected = true

      result shouldBe expected
    }
  }

  "orVerifier" should "fail to verify a proof with invalid a and b sub-proofs" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = 50L.asProposition
        .or(
          sk.vk.asProposition
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
        proof.satisfies[F](proposition)

      val expected = false

      result shouldBe expected
    }
  }

  "thresholdVerifier" should "verify a proof" in {
    forAll { (sk: SecretKeys.Ed25519, sk2: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = List(
        50L.asProposition,
        sk.vk.asProposition,
        sk2.vk.asProposition
      ).threshold(2)

      val proof = Proofs.Compositional.Threshold(
        List(
          Proofs.Contextual.HeightLock(),
          ed25519.sign(sk, unprovenTransaction.signableBytes),
          Proofs.False
        )
      )

      val transaction = unprovenTransaction.prove(_ => proof)

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
        proof.satisfies[F](proposition)

      val expected = true

      result shouldBe expected
    }
  }

  "thresholdVerifier" should "fail to verify if the threshold is not met" in {
    forAll { (sk: SecretKeys.Ed25519, sk2: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven) =>
      val proposition = List(
        50L.asProposition,
        sk.vk.asProposition,
        sk2.vk.asProposition
      ).threshold(2)

      val proof = Proofs.Compositional.Threshold(
        List(
          Proofs.Contextual.HeightLock(),
          ed25519.sign(sk, unprovenTransaction.signableBytes),
          Proofs.False
        )
      )

      val transaction = unprovenTransaction.prove(_ => proof)

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
        proof.satisfies[F](proposition)

      val expected = false

      result shouldBe expected
    }
  }

}

object ProofVerifierSpec {

  type F[A] = Id[A]

  implicit val ed25519: Ed25519 = new Ed25519()
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  implicit val jsExecutor: Propositions.Script.JS.JSScript => F[(Json, Json) => F[Boolean]] =
    (script: Propositions.Script.JS.JSScript) => (verificationCtx: Json, args: Json) => true.pure[F]
}
