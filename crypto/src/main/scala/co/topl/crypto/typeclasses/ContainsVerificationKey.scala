package co.topl.crypto.typeclasses

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.signatures.Ed25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

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

    implicit def ed25519ContainsVerificationKey(implicit
      ed25519: Ed25519
    ): ContainsVerificationKey[SecretKeys.Ed25519, VerificationKeys.Ed25519] =
      key => {
        val pkBytes = new Array[Byte](32)
        ed25519.generatePublicKey(key.bytes.data.toArray, 0, pkBytes, 0)
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(pkBytes)))
      }

    implicit def extendedEd25519ContainsVerificationKey(implicit
      ed25519: Ed25519
    ): ContainsVerificationKey[SecretKeys.ExtendedEd25519, VerificationKeys.ExtendedEd25519] =
      key => {
        val vk = new Array[Byte](ed25519.PUBLIC_KEY_SIZE)
        ed25519.scalarMultBaseEncoded(key.leftKey.data.toArray, vk, 0)
        VerificationKeys.ExtendedEd25519(VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(vk))), key.chainCode)
      }

    implicit def vrfContainsVerificationKey(implicit
      ed25519: Ed25519
    ): ContainsVerificationKey[SecretKeys.Vrf, VerificationKeys.Vrf] =
      key => VerificationKeys.Vrf(ed25519ContainsVerificationKey.verificationKeyOf(key.ed25519))

    implicit val kesContainsVerificationKey
      : ContainsVerificationKey[SecretKeys.SymmetricMMM, VerificationKeys.HdKes] = {
      val scheme = new KeyEvolvingSignatureScheme
      key => ???
    }
  }

  object implicits extends Implicits
  object instances extends Instances
}
