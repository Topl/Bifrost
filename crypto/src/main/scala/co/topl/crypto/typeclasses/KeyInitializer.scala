package co.topl.crypto.typeclasses

import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.SymmetricKey
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signatures.{Ed25519, Ed25519VRF}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import simulacrum.typeclass

import java.nio.charset.StandardCharsets
import java.security.SecureRandom

@typeclass trait KeyInitializer[SK] {
  self =>

  /**
   * Creates a random secret key
   */
  def random(): SK

  /**
   * Creates a secret key from the given seed
   */
  def fromSeed(seed: Bytes): SK
}

object KeyInitializer {

  trait Instances {

    implicit val ed25519Initializer: KeyInitializer[SecretKeys.Ed25519] =
      new KeyInitializer[SecretKeys.Ed25519] {
        private val instance = new Ed25519

        override def random(): SecretKeys.Ed25519 =
          (fromInstanceTypes _).tupled(instance.createKeyPair)

        override def fromSeed(seed: Bytes): SecretKeys.Ed25519 =
          (fromInstanceTypes _).tupled(instance.createKeyPair(seed.toArray))

        private def fromInstanceTypes(priv: co.topl.crypto.PrivateKey, pub: co.topl.crypto.PublicKey) =
          SecretKeys.Ed25519(
            Sized.strictUnsafe[Bytes, SecretKeys.Ed25519.Length](Bytes(priv.value))
          )
      }

    implicit val vrfInitializer: KeyInitializer[SecretKeys.Vrf] =
      new KeyInitializer[SecretKeys.Vrf] {

        def random(): SecretKeys.Vrf =
          (fromLib _).tupled(Ed25519VRF.instance.createKeyPair)

        def fromSeed(seed: Bytes): SecretKeys.Vrf =
          (fromLib _).tupled(Ed25519VRF.instance.createKeyPair(seed.toArray))

        private def fromLib(sk: co.topl.crypto.PrivateKey, pk: co.topl.crypto.PublicKey): SecretKeys.Vrf =
          SecretKeys.Vrf(SecretKeys.Ed25519(Sized.strictUnsafe(Bytes(sk.value))))
      }

    implicit def kesInitializer(implicit slot: Slot): KeyInitializer[SecretKeys.SymmetricMMM] =
      new KeyInitializer[SecretKeys.SymmetricMMM] {
        private val scheme = new KeyEvolvingSignatureScheme

        def random(): SecretKeys.SymmetricMMM =
          SymmetricKey.newFromSeed(
            new SecureRandom().generateSeed(32),
            offset = slot,
            // TODO:
            signer = _ => Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0))))
          )

        def fromSeed(seed: Bytes): SecretKeys.SymmetricMMM =
          SymmetricKey.newFromSeed(seed.toArray, offset = slot, signer = ???)

      }

    implicit def extendedEd25519Initializer(implicit entropy: Entropy): KeyInitializer[SecretKeys.ExtendedEd25519] =
      new KeyInitializer[SecretKeys.ExtendedEd25519] {

        def random(): SecretKeys.ExtendedEd25519 =
          fromSeed(
            Bytes(
              Pbkdf2Sha512.generateKey(
                Array.emptyByteArray,
                entropy.value,
                96,
                4096
              )
            )
          )

        def fromSeed(seed: Bytes): SecretKeys.ExtendedEd25519 = {
          // first do a PBDKF2-HMAC-SHA512 per the SLIP2-0023 spec
          val seed = Pbkdf2Sha512.generateKey(
            Array.emptyByteArray,
            entropy.value,
            96,
            4096
          )
          // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 spec
          seed(0) = (seed(0) & 0xf8).toByte
          seed(31) = ((seed(31) & 0x1f) | 0x40).toByte
          SecretKeys.ExtendedEd25519(
            Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.LeftLength](Bytes(seed.slice(0, 32))),
            Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.RightLength](Bytes(seed.slice(0, 32))),
            Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.ChainCodeLength](Bytes(seed.slice(0, 32)))
          )
        }
      }

    implicit def extendedEd25519PasswordInitializer(implicit
      entropy: Entropy
    ): KeyInitializer[String => SecretKeys.ExtendedEd25519] =
      new KeyInitializer[String => SecretKeys.ExtendedEd25519] {

        def random(): String => SecretKeys.ExtendedEd25519 =
          password =>
            fromSeed(
              Bytes(
                Pbkdf2Sha512.generateKey(
                  password.getBytes(StandardCharsets.UTF_8),
                  entropy.value,
                  96,
                  4096
                )
              )
            )(password)

        def fromSeed(seed: Bytes): String => SecretKeys.ExtendedEd25519 = {
          // The password is already assumed to be encapsulated by the seed
          _ =>
            val seedArray = seed.toArray
            // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 spec
            seedArray(0) = (seedArray(0) & 0xf8).toByte
            seedArray(31) = ((seedArray(31) & 0x1f) | 0x40).toByte
            SecretKeys.ExtendedEd25519(
              Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.LeftLength](Bytes(seedArray.slice(0, 32))),
              Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.RightLength](Bytes(seedArray.slice(0, 32))),
              Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.ChainCodeLength](Bytes(seedArray.slice(0, 32)))
            )
        }
      }

  }
  object Instances extends Instances
}
