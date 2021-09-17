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

    implicit val ed25519ContainsVerificationKey: ContainsVerificationKey[PrivateKeys.Ed25519, PublicKeys.Ed25519] =
      key => {
        val pkBytes = new Array[Byte](32)
        new Ed25519().generatePublicKey(key.bytes.data.toArray, 0, pkBytes, 0)
        PublicKeys.Ed25519(Sized.strictUnsafe(Bytes(pkBytes)))
      }

    implicit val vrfContainsVerificationKey: ContainsVerificationKey[PrivateKeys.Vrf, PublicKeys.Vrf] =
      key => PublicKeys.Vrf(ed25519ContainsVerificationKey.verificationKeyOf(key.ed25519))

    implicit val kesContainsVerificationKey: ContainsVerificationKey[PrivateKeys.Kes, PublicKeys.Kes] = {
      val scheme = new KeyEvolvingSignatureScheme
      key => {
        val symmetricKey =
          SymmetricKey(
            ProductPrivateKey.deserializeProductKey(
              Ints.toByteArray(
                key.bytes.data.toArray.length
              ) ++ key.bytes.data.toArray
            )
          )
        PublicKeys.Kes(Sized.strictUnsafe(Bytes(scheme.publicKey(symmetricKey))), symmetricKey.data.offset)
      }
    }
  }

  object instances extends Instances
}
