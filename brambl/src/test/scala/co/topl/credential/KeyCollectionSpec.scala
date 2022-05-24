package co.topl.credential

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signing.{Ed25519, EntropyToSeed, ExtendedEd25519, Password}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import java.nio.charset.StandardCharsets

class KeyCollectionSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with OptionValues
    with EitherValues
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  import KeyCollectionSpec._

  behavior of "RefKeyCollection"

  type F[A] = IO[A]

  it should "unlock keys from a CredentialIO" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      val evidence = sk.vk.typedEvidence

      (credentialIO
        .unlock(_, _))
        .expects(evidence, password)
        .once()
        .returning((sk.persistedBytes -> KeyFile.Metadata(evidence)).some.pure[F])

      val underTest = RefKeyCollection[F].unsafeRunSync()

      underTest.unlock[SecretKeys.ExtendedEd25519](evidence, password).unsafeRunSync().value shouldBe sk

    }
  }

  it should "list unlocked entries" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      val evidence = sk.vk.typedEvidence

      (credentialIO
        .unlock(_, _))
        .expects(evidence, password)
        .once()
        .returning((sk.persistedBytes -> KeyFile.Metadata(evidence)).some.pure[F])

      val underTest = RefKeyCollection[F].unsafeRunSync()

      underTest.unlock[SecretKeys.ExtendedEd25519](evidence, password).unsafeRunSync().value shouldBe sk

      underTest.unlockedEntries.unsafeRunSync() shouldBe Set(evidence)
    }
  }

  it should "return an already unlocked entry" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      val evidence = sk.vk.typedEvidence

      (credentialIO
        .unlock(_, _))
        .expects(evidence, password)
        .once()
        .returning((sk.persistedBytes -> KeyFile.Metadata(evidence)).some.pure[F])

      val underTest = RefKeyCollection[F].unsafeRunSync()

      underTest.unlock[SecretKeys.ExtendedEd25519](evidence, password).unsafeRunSync().value shouldBe sk

      underTest.lift(evidence).unsafeRunSync().value shouldBe sk
    }
  }
}

object KeyCollectionSpec {

  implicit val arbitraryBytes: Arbitrary[Bytes] =
    Arbitrary(
      Gen.alphaNumStr.map(_.getBytes(StandardCharsets.UTF_8)).map(Bytes(_))
    )

  implicit val arbitraryExtendedEd25519: Arbitrary[SecretKeys.ExtendedEd25519] =
    Arbitrary(
      Arbitrary.arbUuid.arbitrary
        .map(Entropy.fromUuid)
        .map(new ExtendedEd25519().createKeyPair(_, None)._1)
    )

  implicit val arbitraryEd25519: Arbitrary[SecretKeys.Ed25519] = {
    implicit val entropyToSeed: EntropyToSeed[SecretKeys.Ed25519.Length] =
      (entropy, _) => Sized.strictUnsafe(Bytes(blake2b256.hash(entropy.value).value))

    Arbitrary(
      Arbitrary.arbUuid.arbitrary
        .map(Entropy.fromUuid)
        .map(new Ed25519().createKeyPair(_, None)._1)
    )
  }
}
