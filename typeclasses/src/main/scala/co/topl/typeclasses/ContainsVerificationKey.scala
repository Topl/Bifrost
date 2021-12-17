package co.topl.typeclasses

import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF, ExtendedEd25519}
import co.topl.models._

/**
 * Indicates that some value T can produce a Verification Key
 */
@simulacrum.typeclass
trait ContainsVerificationKey[T] {

  /**
   * Constructs a verification key using the given value T
   */
  @simulacrum.op("vk")
  def verificationKeyOf(t: T): VerificationKey
}

object ContainsVerificationKey {

  trait Instances {

    implicit val curve25519ContainsVerificationKey: ContainsVerificationKey[SecretKeys.Curve25519] =
      key => {
        new Curve25519().getVerificationKey(key)
      }

    implicit val ed25519ContainsVerificationKey: ContainsVerificationKey[SecretKeys.Ed25519] =
      key => {
        new Ed25519().getVerificationKey(key)
      }

    implicit val extendedEd25519ContainsVerificationKey: ContainsVerificationKey[SecretKeys.ExtendedEd25519] =
      key => {
        new ExtendedEd25519().getVerificationKey(key)
      }

    implicit val vrfContainsVerificationKey: ContainsVerificationKey[SecretKeys.VrfEd25519] =
      key => {
        new Ed25519VRF().getVerificationKey(key)
      }
  }

  object instances extends Instances
}
