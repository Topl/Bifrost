package co.topl.credential

import cats.implicits._
import cats.implicits._
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.typeclasses.VerificationContext
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import io.circe.Json
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.collection.immutable.ListMap

class CredentialSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  import CredentialSpec._

  "CurveSigningCredential" should "create a proposition and proof" in {
    forAll { (sk: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven) =>
      val credential = Credential.Knowledge.Curve25519(sk, unprovenTransaction)

      credential.proposition shouldBe sk.vk.asProposition
      Curve25519.instance.verify(
        credential.proof.asInstanceOf[Proofs.Knowledge.Curve25519],
        unprovenTransaction.signableBytes,
        sk.vk.asInstanceOf[VerificationKeys.Curve25519]
      ) shouldBe true
    }
  }

  "EdSigningCredential" should "create a proposition and proof" in {
    forAll { (sk: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven) =>
      val credential = Credential.Knowledge.Ed25519(sk, unprovenTransaction)

      credential.proposition shouldBe sk.vk.asProposition
      ed25519.verify(
        credential.proof.asInstanceOf[Proofs.Knowledge.Ed25519],
        unprovenTransaction.signableBytes,
        sk.vk.asInstanceOf[VerificationKeys.Ed25519]
      ) shouldBe true
    }
  }

  "ExtendedEdSigningCredential" should "create a proposition and proof" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, unprovenTransaction: Transaction.Unproven) =>
      val credential = Credential.Knowledge.ExtendedEd25519(sk, unprovenTransaction)

      credential.proposition shouldBe sk.vk.asProposition
      extendedEd25519.verify(
        credential.proof.asInstanceOf[Proofs.Knowledge.Ed25519],
        unprovenTransaction.signableBytes,
        sk.vk.asInstanceOf[VerificationKeys.ExtendedEd25519]
      ) shouldBe true
    }
  }

  "HeightLockCredential" should "create a proposition and proof" in {
    forAll { height: Long =>
      val credential = Credential.Contextual.HeightLock(height)
      credential.proposition shouldBe Propositions.Contextual.HeightLock(height)
      credential.proof shouldBe Proofs.Contextual.HeightLock()
    }
  }

  "AndCredential" should "create a proposition and proof" in {
    forAll { (sk: SecretKeys.Curve25519, unprovenTransaction: Transaction.Unproven, height: Long) =>
      whenever(height >= 0 && height < Long.MaxValue) {
        val skCredential = Credential.Knowledge.Curve25519(sk, unprovenTransaction)
        val heightCredential = Credential.Contextual.HeightLock(height)

        val andProposition = skCredential.proposition.and(heightCredential.proposition)
        val andCredential = Credential.Compositional.And(andProposition, List(skCredential, heightCredential))

        andCredential.proposition shouldBe andProposition

        val andProof = andCredential.proof

        val transaction =
          Transaction(
            ListMap.from(unprovenTransaction.inputs.map(boxRef => boxRef -> (andProposition -> andProof))),
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
          .returning(height + 1)

        andProof.satisfies[F](andProposition) shouldBe true

      }
    }
  }

  "OrCredential" should "create a proposition and proof" in {
    forAll { (sk: SecretKeys.Curve25519, height: Long) =>
      whenever(height >= 0 && height < Long.MaxValue) {
        val heightCredential = Credential.Contextual.HeightLock(height)

        val orProposition = sk.vk.asProposition
          .or(heightCredential.proposition)
        val orCredential = Credential.Compositional.Or(orProposition, List(heightCredential))

        orCredential.proposition shouldBe orProposition

        val orProof = orCredential.proof

        implicit val context: VerificationContext[F] = mock[VerificationContext[F]]

        (() => context.currentHeight)
          .expects()
          .once()
          .returning(height + 1)

        orProof.satisfies[F](orProposition) shouldBe true
      }
    }
  }

  "ThresholdCredential" should "create a proposition and proof" in {
    forAll {
      (sk: SecretKeys.Curve25519, sk2: SecretKeys.Ed25519, unprovenTransaction: Transaction.Unproven, height: Long) =>
        whenever(height >= 0 && height < Long.MaxValue) {
          val skCredential = Credential.Knowledge.Curve25519(sk, unprovenTransaction)
          val heightCredential = Credential.Contextual.HeightLock(height)

          val thresholdProposition =
            List(
              sk.vk.asProposition,
              sk2.vk.asProposition,
              heightCredential.proposition
            ).threshold(2)

          val thresholdCredential =
            Credential.Compositional.Threshold(thresholdProposition, List(skCredential, heightCredential))

          thresholdCredential.proposition shouldBe thresholdProposition

          val thresholdProof = thresholdCredential.proof

          val transaction =
            Transaction(
              ListMap.from(
                unprovenTransaction.inputs.map(boxRef => boxRef -> (thresholdProposition -> thresholdProof))
              ),
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
            .returning(height + 1)

          thresholdProposition isSatisfiedBy thresholdProof shouldBe true
        }
    }
  }

  "Complex Credentials" should "be formed" in {
    forAll {
      (
        sk:                  SecretKeys.Curve25519,
        vk2:                 VerificationKeys.Ed25519,
        sk3:                 SecretKeys.Ed25519,
        vk4:                 VerificationKeys.Ed25519,
        unprovenTransaction: Transaction.Unproven,
        height:              Long
      ) =>
        whenever(height >= 0 && height < Long.MaxValue) {
          val skCredential = Credential.Knowledge.Curve25519(sk, unprovenTransaction)
          val sk3Credential = Credential.Knowledge.Ed25519(sk3, unprovenTransaction)
          val heightCredential = Credential.Contextual.HeightLock(height)

          val thresholdProposition =
            List(skCredential.proposition, vk2.asProposition, sk3Credential.proposition)
              .threshold(2)

          val orProposition =
            vk4.asProposition or heightCredential.proposition

          val andProposition =
            thresholdProposition and orProposition

          val andCredential =
            Credential.Compositional.And(andProposition, List(skCredential, sk3Credential, heightCredential))

          val andProof =
            andCredential.proof

          val transaction =
            Transaction(
              ListMap.from(
                unprovenTransaction.inputs.map(boxRef => boxRef -> (andProposition -> andProof))
              ),
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
            .anyNumberOfTimes()
            .returning(transaction)

          (() => context.currentHeight)
            .expects()
            .once()
            .returning(height + 1)

          andProof.satisfies[F](andProposition)
        }
    }
  }

}

object CredentialSpec {

  type F[A] = cats.Id[A]

  implicit val ed25519: Ed25519 = new Ed25519()
  implicit val extendedEd25519: ExtendedEd25519 = new ExtendedEd25519

  implicit val jsExecutor: Propositions.Script.JS.JSScript => F[(Json, Json) => F[Boolean]] =
    (script: Propositions.Script.JS.JSScript) => (verificationCtx: Json, args: Json) => true.pure[F]
}
