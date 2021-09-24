package co.topl.crypto.typeclasses

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.signatures.Ed25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

trait ContainsVerificationKey[T, VK] {
  def verificationKeyOf(privateKey: T): VK
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
    }
  }

  trait Instances {

    private val sharedEd25519 = new Ed25519()

    implicit val ed25519ContainsVerificationKey: ContainsVerificationKey[SecretKeys.Ed25519, VerificationKeys.Ed25519] =
      key => {
        val pkBytes = new Array[Byte](32)
        sharedEd25519.generatePublicKey(key.bytes.data.toArray, 0, pkBytes, 0)
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(pkBytes)))
      }

    implicit val extendedEd25519ContainsVerificationKey
      : ContainsVerificationKey[SecretKeys.ExtendedEd25519, VerificationKeys.ExtendedEd25519] =
      key => {
        val vk = new Array[Byte](sharedEd25519.PUBLIC_KEY_SIZE)
        sharedEd25519.scalarMultBaseEncoded(key.leftKey.data.toArray, vk, 0)
        VerificationKeys.ExtendedEd25519(VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(vk))), key.chainCode)
      }

    implicit val vrfContainsVerificationKey: ContainsVerificationKey[SecretKeys.Vrf, VerificationKeys.Vrf] =
      key => VerificationKeys.Vrf(ed25519ContainsVerificationKey.verificationKeyOf(key.ed25519))

    implicit val kesContainsVerificationKey
      : ContainsVerificationKey[SecretKeys.SymmetricMMM, VerificationKeys.HdKes] = {
      val scheme = new KeyEvolvingSignatureScheme
      key => VerificationKeys.HdKes(Sized.strictUnsafe(Bytes(scheme.publicKey(key))))
    }
  }

  object implicits extends Implicits
  object instances extends Instances
}
