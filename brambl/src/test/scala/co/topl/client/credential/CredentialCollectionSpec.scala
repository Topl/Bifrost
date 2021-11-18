package co.topl.client.credential

import cats.Applicative
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.mnemonic.Bip32Indexes.Bip32IndexesSupport
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signing.{Ed25519, EntropyToSeed, ExtendedEd25519, Password}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import org.scalacheck.Arbitrary
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class CredentialCollectionSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks {

  import CredentialCollectionSpec._
  implicit val networkPrefix: NetworkPrefix = NetworkPrefix(1: Byte)

  type F[A] = IO[A]

  behavior of "ToplCredentialTree"

  it should "initialize from a SK" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password, credentialSetName: String) =>
      whenever(credentialSetName.nonEmpty) {
        implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

        (credentialIO.write _)
          .expects(credentialSetName, sk.dionAddress, *, password)
          .once()
          .returning(Applicative[F].unit)

        val tree =
          ToplCredentialTree.create[F](credentialSetName, sk, password).unsafeRunSync()

        tree.name shouldBe credentialSetName

        val account =
          tree / 0L.hardened

        account.name shouldBe credentialSetName
        account.accountSK === sk shouldBe false
      }
    }
  }

  behavior of "AccountCredentialTree"

  it should "initialize from a SK" in {
    forAll { (sk: SecretKeys.ExtendedEd25519, password: Password, credentialSetName: String) =>
      whenever(credentialSetName.nonEmpty) {
        implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

        (credentialIO.write _)
          .expects(credentialSetName, sk.dionAddress, *, password)
          .once()
          .returning(Applicative[F].unit)

        val account =
          AccountCredentialTree.create[F](credentialSetName, sk, 0L, password).unsafeRunSync()

        account.name shouldBe credentialSetName

        val role =
          account.role(0L.soft)

        role.roleSK === sk shouldBe false

        role.credential(0L.soft)
      }
    }
  }

  behavior of "CredentialSet"

  it should "initialize an empty set" in {
    forAll { credentialSetName: String =>
      whenever(credentialSetName.nonEmpty) {
        val set = CredentialSet[Proposition](credentialSetName)

        set.name shouldBe credentialSetName
        set.credentials should be(empty)
      }
    }
  }

  it should "add and remove credentials" in {

    forAll { (sk: SecretKeys.Ed25519, password: Password, credentialSetName: String) =>
      whenever(credentialSetName.nonEmpty) {
        val set = CredentialSet[Propositions.Knowledge.Ed25519](credentialSetName)
        val credential = Credential(sk)

        implicit val credentialIO: CredentialIO[F] = mock[CredentialIO[F]]

        (credentialIO.write _)
          .expects(credentialSetName, sk.dionAddress, *, password)
          .once()
          .returning(Applicative[F].unit)

        val credentialBytes =
          sk.bytes.data ++ sk.verificationKey[VerificationKeys.Ed25519].bytes.data

        val address =
          credential.address

        val newSet =
          set.persistAndInclude[F](credential, credentialBytes, password).unsafeRunSync()

        newSet.credentials.values.exists(_.address === address) shouldBe true

      }
    }
  }
}

object CredentialCollectionSpec {

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
