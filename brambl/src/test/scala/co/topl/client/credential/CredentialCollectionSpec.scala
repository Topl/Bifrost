package co.topl.client.credential

import cats.Applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.client.credential.Credential.instances.ed25519SKEncoder
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.mnemonic.Bip32Indexes.Bip32IndexesSupport
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signing.{Ed25519, EntropyToSeed, ExtendedEd25519, Password}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import java.nio.charset.StandardCharsets

class CredentialCollectionSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with OptionValues
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  import CredentialCollectionSpec._
  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)
  implicit private val ed25519: Ed25519 = new Ed25519
  implicit private val extendedEd25519: ExtendedEd25519 = ExtendedEd25519.precomputed()

  type F[A] = IO[A]

  behavior of "HierarchicalCredentialCollection"

  it should "ignore malformed data" in {
    forAll { (bytes: Bytes, sk: SecretKeys.ExtendedEd25519, password: Password) =>
      whenever(bytes.length != 128L) {
        implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

        val evidence = sk.dionAddress.typedEvidence.evidence

        (credentialIO.unlock _)
          .expects(evidence, password)
          .once()
          .returning(
            (bytes, KeyFile.Metadata(evidence, KeyFile.Metadata.ExtendedEd25519)).some
              .pure[F]
          )

        HierarchicalCredentialCollection.load[F](evidence, password).unsafeRunSync() shouldBe None
      }
    }
  }

  it should "ignore non-Topl hierarchies" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      val evidence = sk.dionAddress.typedEvidence.evidence

      val bytes = sk.leftKey.data ++ sk.rightKey.data ++ sk.chainCode.data ++
        Bytes(Longs.toByteArray(50L)) ++
        Bytes(Longs.toByteArray(50L)) ++
        Bytes(Longs.toByteArray(50L)) ++
        Bytes(Longs.toByteArray(50L))

      (credentialIO.unlock _)
        .expects(evidence, password)
        .once()
        .returning(
          (bytes, KeyFile.Metadata(evidence, KeyFile.Metadata.ExtendedEd25519)).some
            .pure[F]
        )

      HierarchicalCredentialCollection.load[F](evidence, password).unsafeRunSync() shouldBe None
    }
  }

  it should "load a ToplCredentialTree from CredentialIO" in {

    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      val evidence = sk.dionAddress.typedEvidence.evidence

      val bytes = sk.leftKey.data ++ sk.rightKey.data ++ sk.chainCode.data ++
        Bytes(Longs.toByteArray(1852L)) ++
        Bytes(Longs.toByteArray(7091L)) ++
        Bytes(Longs.toByteArray(-1L)) ++
        Bytes(Longs.toByteArray(-1L))

      (credentialIO.unlock _)
        .expects(evidence, password)
        .once()
        .returning(
          (bytes, KeyFile.Metadata(evidence, KeyFile.Metadata.ExtendedEd25519)).some
            .pure[F]
        )

      val tree =
        HierarchicalCredentialCollection.load[F](evidence, password).unsafeRunSync().value

      tree shouldBe a[ToplCredentialTree]
    }
  }

  it should "load an AccountCredentialTree from CredentialIO" in {

    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      val evidence = sk.dionAddress.typedEvidence.evidence

      val bytes = sk.leftKey.data ++ sk.rightKey.data ++ sk.chainCode.data ++
        Bytes(Longs.toByteArray(1852L)) ++
        Bytes(Longs.toByteArray(7091L)) ++
        Bytes(Longs.toByteArray(50L)) ++
        Bytes(Longs.toByteArray(-1L))

      (credentialIO.unlock _)
        .expects(evidence, password)
        .once()
        .returning(
          (bytes, KeyFile.Metadata(evidence, KeyFile.Metadata.ExtendedEd25519)).some
            .pure[F]
        )

      val tree =
        HierarchicalCredentialCollection.load[F](evidence, password).unsafeRunSync().value

      tree shouldBe a[AccountCredentialTree]
    }
  }

  behavior of "ToplCredentialTree"

  it should "initialize from a SK" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      (credentialIO.write _)
        .expects(sk.dionAddress.typedEvidence.evidence, *, *, password)
        .once()
        .returning(Applicative[F].unit)

      val tree =
        ToplCredentialTree.persisted[F](sk, password).unsafeRunSync()

      val account =
        tree / 0L.hardened

      account.accountSK === sk shouldBe false
    }
  }

  behavior of "AccountCredentialTree"

  it should "initialize from a SK" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password) =>
      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      (credentialIO.write _)
        .expects(sk.dionAddress.typedEvidence.evidence, *, *, password)
        .once()
        .returning(Applicative[F].unit)

      val account =
        AccountCredentialTree(sk, 0L)

      account.persist[F](password).unsafeRunSync()

      val role =
        account.role(0L.soft)

      role.roleSK === sk shouldBe false

      role.credential(0L.soft)
    }
  }

  behavior of "CredentialSet"

  it should "initialize an empty set" in {
    forAll { credentialSetName: String =>
      whenever(credentialSetName.nonEmpty) {
        val set = CredentialSet.empty

        set.credentials should be(empty)
      }
    }
  }

  it should "add and remove credentials" in {

    forAll { (sk: SecretKeys.Ed25519, password: Password) =>
      val set = CredentialSet.empty
      val credential = Credential(sk)

      implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

      (credentialIO.write _)
        .expects(sk.dionAddress.typedEvidence.evidence, *, *, password)
        .once()
        .returning(Applicative[F].unit)

      val address =
        credential.address

      val newSet =
        set.withPersistentCredential[F](credential, KeyFile.Metadata.Ed25519, password).unsafeRunSync()

      newSet.credentials.values.exists(_.address === address) shouldBe true
    }
  }
}

object CredentialCollectionSpec {

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
