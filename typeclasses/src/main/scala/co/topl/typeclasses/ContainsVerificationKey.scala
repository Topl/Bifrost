package co.topl.typeclasses

import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, ExtendedEd25519}
import co.topl.models._

/**
 * Indicates that some value T can produce a Verification Key
 */
trait ContainsVerificationKey[T, VK] {

  /**
   * Constructs a verification key using the given value T
   */
  def verificationKeyOf(t: T): VK
}

object ContainsVerificationKey {

  def apply[SK, VK](implicit
    containsVerificationKey: ContainsVerificationKey[SK, VK]
  ): ContainsVerificationKey[SK, VK] =
    containsVerificationKey

  trait Implicits {

    implicit class ContainsVerificationKeyOps[SK](t: SK) {

      def verificationKey[VK](implicit containsVerificationKey: ContainsVerificationKey[SK, VK]): VK =
        containsVerificationKey.verificationKeyOf(t)

      def vk[VK](implicit containsVerificationKey: ContainsVerificationKey[SK, VK]): VK =
        verificationKey

    }
  }

  trait Instances {

    implicit val curve25519ContainsVerificationKey
      : ContainsVerificationKey[SecretKeys.Curve25519, VerificationKeys.Curve25519] =
      key => {
        new Curve25519().getVerificationKey(key)
      }

    implicit val ed25519ContainsVerificationKey: ContainsVerificationKey[SecretKeys.Ed25519, VerificationKeys.Ed25519] =
      key => {
        new Ed25519().getVerificationKey(key)
      }

    implicit val extendedEd25519ContainsVerificationKey
      : ContainsVerificationKey[SecretKeys.ExtendedEd25519, VerificationKeys.ExtendedEd25519] =
      key => {
        new ExtendedEd25519().getVerificationKey(key)
      }

    implicit val vrfContainsVerificationKey
      : ContainsVerificationKey[SecretKeys.VrfEd25519, VerificationKeys.VrfEd25519] =
      key => {
        new Ed25519VRF().getVerificationKey(key)
      }

//    implicit val kesContainsVerificationKey
//      : ContainsVerificationKey[SecretKeys.SymmetricMMM, VerificationKeys.HdKes] = {
//      val scheme = new KeyEvolvingSignatureScheme
//      key => ???
//    }
  }

  object implicits extends Implicits
  object instances extends Instances
}
