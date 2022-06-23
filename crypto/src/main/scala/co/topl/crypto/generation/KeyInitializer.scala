package co.topl.crypto.generation

import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing._
import co.topl.models._
import simulacrum.typeclass

import java.util.UUID

@typeclass trait KeyInitializer[SK] {
  self =>

  /**
   * Creates a random secret key
   */
  def random(): SK

  /**
   * Creates a secret key from the given seed
   */
  def fromEntropy(entropy: Entropy, password: Option[Password]): SK
}

object KeyInitializer {

  trait Instances {

    // todo: should these use there own instances or the global instance?
    implicit val curve25519Initializer: KeyInitializer[SecretKeys.Curve25519] =
      new KeyInitializer[SecretKeys.Curve25519] {

        override def random(): SecretKeys.Curve25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        override def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.Curve25519 =
          Curve25519.instance.deriveKeyPairFromEntropy(entropy, password)._1

      }

    implicit val ed25519Initializer: KeyInitializer[SecretKeys.Ed25519] =
      new KeyInitializer[SecretKeys.Ed25519] {

        override def random(): SecretKeys.Ed25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        override def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.Ed25519 =
          Ed25519.instance.deriveKeyPairFromEntropy(entropy, password)._1

      }

    implicit def vrfInitializer(implicit ed25519VRF: Ed25519VRF): KeyInitializer[SecretKeys.VrfEd25519] =
      new KeyInitializer[SecretKeys.VrfEd25519] {

        def random(): SecretKeys.VrfEd25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.VrfEd25519 =
          ed25519VRF.deriveKeyPairFromEntropy(entropy, password)._1
      }

    implicit def extendedEd25519Initializer: KeyInitializer[SecretKeys.ExtendedEd25519] =
      new KeyInitializer[SecretKeys.ExtendedEd25519] {

        def random(): SecretKeys.ExtendedEd25519 =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        def fromEntropy(entropy: Entropy, password: Option[Password]): SecretKeys.ExtendedEd25519 =
          ExtendedEd25519.instance.deriveKeyPairFromEntropy(entropy, password)._1
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
