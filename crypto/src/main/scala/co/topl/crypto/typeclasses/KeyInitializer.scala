package co.topl.crypto.typeclasses

import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, Seed}
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

    implicit val curve25519Initializer: KeyInitializer[SecretKeys.Curve25519] =
      new KeyInitializer[SecretKeys.Curve25519] {

        override def random(): SecretKeys.Curve25519 =
          Curve25519.instance.createKeyPair._1

        override def fromSeed(seed: Bytes): SecretKeys.Curve25519 =
          Curve25519.instance.createKeyPair(Seed(seed.toArray))._1

      }

    implicit val ed25519Initializer: KeyInitializer[SecretKeys.Ed25519] =
      new KeyInitializer[SecretKeys.Ed25519] {

        override def random(): SecretKeys.Ed25519 =
          Ed25519.instance.createKeyPair._1

        override def fromSeed(seed: Bytes): SecretKeys.Ed25519 =
          Ed25519.instance.createKeyPair(Seed(seed.toArray))._1

      }

    implicit val vrfInitializer: KeyInitializer[SecretKeys.VrfEd25519] =
      new KeyInitializer[SecretKeys.VrfEd25519] {

        def random(): SecretKeys.VrfEd25519 = Ed25519VRF.instance.createKeyPair._1

        def fromSeed(seed: Bytes): SecretKeys.VrfEd25519 =
          Ed25519VRF.instance.createKeyPair(Seed(seed.toArray))._1
      }

    implicit def extendedEd25519Initializer: KeyInitializer[SecretKeys.ExtendedEd25519] =
      new KeyInitializer[SecretKeys.ExtendedEd25519] {

        def random(): SecretKeys.ExtendedEd25519 =
          fromSeed(Bytes(SecureRandom.getSeed(128)))

        def fromSeed(seed: Bytes): SecretKeys.ExtendedEd25519 = {
          // first do a PBDKF2-HMAC-SHA512 per the SLIP-0023 spec
          val seedArray = Pbkdf2Sha512.generateKey(
            Array.emptyByteArray,
            seed.toArray,
            96,
            4096
          )
          // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 spec
          seedArray(0) = (seedArray(0) & 0xf8).toByte
          seedArray(31) = ((seedArray(31) & 0x1f) | 0x40).toByte

          SecretKeys.ExtendedEd25519(
            Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.LeftLength](Bytes(seedArray.slice(0, 32))),
            Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.RightLength](Bytes(seedArray.slice(32, 64))),
            Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.ChainCodeLength](Bytes(seedArray.slice(64, 96)))
          )
        }
      }

    implicit def extendedEd25519PasswordInitializer: KeyInitializer[String => SecretKeys.ExtendedEd25519] =
      new KeyInitializer[String => SecretKeys.ExtendedEd25519] {

        def random(): String => SecretKeys.ExtendedEd25519 =
          password =>
            fromSeed(
              Bytes(
                Pbkdf2Sha512.generateKey(
                  password.getBytes(StandardCharsets.UTF_8),
                  SecureRandom.getSeed(128),
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
              Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.RightLength](Bytes(seedArray.slice(32, 64))),
              Sized.strictUnsafe[Bytes, SecretKeys.ExtendedEd25519.ChainCodeLength](Bytes(seedArray.slice(64, 96)))
            )
        }
      }

//    implicit def kesSumInitializer(implicit slot: Slot): KeyInitializer[SecretKeys.KesSum] =
//      new KeyInitializer[SecretKeys.KesSum] {
//        private val scheme = new KeyEvolvingSignatureScheme
//
//        def random(): SecretKeys.KesSum =
//          SymmetricKey.newFromSeed(
//            new SecureRandom().generateSeed(32),
//            offset = slot,
//            // TODO:
//            signer = _ => Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0))))
//          )
//
//        def fromSeed(seed: Bytes): SecretKeys.KesSum =
//          SymmetricKey.newFromSeed(seed.toArray, offset = slot, signer = ???)
//
//      }

  }
  object Instances extends Instances
}
