package co.topl.crypto.typeclasses

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.{ProductPrivateKey, SymmetricKey}
import co.topl.crypto.signatures.Ed25519
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.models._
import com.google.common.primitives.Ints

trait ContainsVerificationKey[T, VK] {
  def verificationKeyOf(privateKey: T): VK
}

object ContainsVerificationKey {

  def apply[SK, VK](implicit
    containsVerificationKey: ContainsVerificationKey[SK, VK]
  ): ContainsVerificationKey[SK, VK] =
    containsVerificationKey

  trait Instances {

    private val sharedEd25519 = new Ed25519()

    implicit val ed25519ContainsVerificationKey: ContainsVerificationKey[PrivateKeys.Ed25519, PublicKeys.Ed25519] =
      key => {
        val pkBytes = new Array[Byte](32)
        sharedEd25519.generatePublicKey(key.bytes.data.toArray, 0, pkBytes, 0)
        PublicKeys.Ed25519(Sized.strictUnsafe(Bytes(pkBytes)))
      }

    implicit val extendedEd25519ContainsVerificationKey
      : ContainsVerificationKey[PrivateKeys.ExtendedEd25519, PublicKeys.ExtendedEd25519] =
      key => {
        val vk = new Array[Byte](sharedEd25519.PUBLIC_KEY_SIZE)
        sharedEd25519.scalarMultBaseEncoded(key.leftKey.data.toArray, vk, 0)
        PublicKeys.ExtendedEd25519(Sized.strictUnsafe(Bytes(vk)), key.chainCode)
      }

    implicit val vrfContainsVerificationKey: ContainsVerificationKey[PrivateKeys.Vrf, PublicKeys.Vrf] =
      key => PublicKeys.Vrf(ed25519ContainsVerificationKey.verificationKeyOf(key.ed25519))

    implicit val kesContainsVerificationKey: ContainsVerificationKey[PrivateKeys.Kes, PublicKeys.Kes] = {
      val scheme = new KeyEvolvingSignatureScheme
      key => {
        val keyBytesArray = key.bytes.data.toArray
        val symmetricKey = SymmetricKey.deserializeSymmetricKey(Ints.toByteArray(keyBytesArray.length) ++ keyBytesArray)
        PublicKeys.Kes(Sized.strictUnsafe(Bytes(scheme.publicKey(symmetricKey))), symmetricKey.data.offset)
      }
    }
  }

  object instances extends Instances
}
