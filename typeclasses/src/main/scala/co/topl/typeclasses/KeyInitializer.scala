package co.topl.typeclasses

import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signing._
import co.topl.models._
import simulacrum.typeclass

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

    //todo: should these use there own instances or the global instance?
    implicit val curve25519Initializer: KeyInitializer[SecretKeys.Curve25519] =
      new KeyInitializer[SecretKeys.Curve25519] {

        override def random(): SecretKeys.Curve25519 =
          fromSeed(Bytes(defaultRandom))

        override def fromSeed(seed: Bytes): SecretKeys.Curve25519 =
          Curve25519.instance.createKeyPair(Seed(seed.toArray))._1

      }

    implicit val ed25519Initializer: KeyInitializer[SecretKeys.Ed25519] =
      new KeyInitializer[SecretKeys.Ed25519] {

        override def random(): SecretKeys.Ed25519 =
          fromSeed(Bytes(defaultRandom))

        override def fromSeed(seed: Bytes): SecretKeys.Ed25519 =
          Ed25519.instance.createKeyPair(Seed(seed.toArray))._1

      }

    implicit val vrfInitializer: KeyInitializer[SecretKeys.VrfEd25519] =
      new KeyInitializer[SecretKeys.VrfEd25519] {

        def random(): SecretKeys.VrfEd25519 =
          fromSeed(Bytes(defaultRandom))

        def fromSeed(seed: Bytes): SecretKeys.VrfEd25519 =
          Ed25519VRF.instance.createKeyPair(Seed(seed.toArray))._1
      }

    implicit val extendedEd25519Initializer: KeyInitializer[SecretKeys.ExtendedEd25519] =
      new KeyInitializer[SecretKeys.ExtendedEd25519] {

        // here for compatibility with signing routines, assumes password = ""
        def random(): SecretKeys.ExtendedEd25519 =
          fromSeed(Bytes(defaultRandom))

        def fromSeed(seed: Bytes): SecretKeys.ExtendedEd25519 =
          ExtendedEd25519.instance.createKeyPair(Seed(seed.toArray))._1
      }

    implicit def extendedEd25519PasswordInitializer: KeyInitializer[String => SecretKeys.ExtendedEd25519] =
      new KeyInitializer[String => SecretKeys.ExtendedEd25519] {

        def random(): String => SecretKeys.ExtendedEd25519 =
          password => fromSeed(Bytes(defaultRandom))(password)

        def fromSeed(seed: Bytes): String => SecretKeys.ExtendedEd25519 =
          password => ExtendedEd25519.fromEntropy(Entropy(seed.toArray))(password)
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
