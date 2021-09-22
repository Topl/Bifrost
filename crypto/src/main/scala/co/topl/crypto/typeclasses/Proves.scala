package co.topl.crypto.typeclasses

import co.topl.crypto.signatures.eddsa.Ed25519
import co.topl.crypto.typeclasses.ContainsVerificationKey.instances._
import co.topl.crypto.typeclasses.Signable.ops._
import co.topl.crypto.typeclasses.implicits.ContainsVerificationKeyOps
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

import scala.language.implicitConversions

trait Proves[T, Prf <: Proof] {
  def proveWith[Data: Signable](t: T, data: Data): Prf
}

object Proves {

  def apply[T, Prf <: Proof](implicit p: Proves[T, Prf]): Proves[T, Prf] = p

  trait Instances {

    implicit val extendedEd25519Proves: Proves[SecretKeys.ExtendedEd25519, Proofs.Signature.Ed25519] =
      new Proves[SecretKeys.ExtendedEd25519, Proofs.Signature.Ed25519] {
        private val ed = new Ed25519

        def proveWith[Data: Signable](t: SecretKeys.ExtendedEd25519, data: Data): Proofs.Signature.Ed25519 = {
          val signatureArray: Array[Byte] = new Array[Byte](ed.SIGNATURE_SIZE)
          val ctx: Array[Byte] = Array.emptyByteArray
          val phflag: Byte = 0x00
          val h: Array[Byte] = (t.leftKey.data ++ t.rightKey.data).toArray
          val s: Array[Byte] = t.leftKey.data.toArray
          val vk = t.verificationKey[VerificationKeys.ExtendedEd25519]
          val pk: Array[Byte] = (vk.ed25519.bytes.data ++ vk.chainCode.data).toArray
          val m: Array[Byte] = data.signableBytes.toArray
          ed.implSign(ed.sha512Digest, h, s, pk, 0, ctx, phflag, m, 0, m.length, signatureArray, 0)
          Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(signatureArray)))
        }
      }

    implicit val kesPrivateKeyProves: Proves[SecretKeys.SymmetricMMM, Proofs.Signature.HdKes] =
      new Proves[SecretKeys.SymmetricMMM, Proofs.Signature.HdKes] {

        def proveWith[Data: Signable](t: SecretKeys.SymmetricMMM, data: Data): Proofs.Signature.HdKes =
          Proofs.Signature.HdKes(
            i = 0,
            vkI = ???,
            ecSignature = ???,
            sigSumJ = ???,
            sigSumK = ???
          )
      }
  }
  object instances extends Instances
}
