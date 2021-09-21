package co.topl.crypto.typeclasses

import co.topl.crypto.signatures.eddsa.Ed25519
import co.topl.crypto.typeclasses.ContainsVerificationKey.instances._
import co.topl.crypto.typeclasses.Signable.ops._
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

    implicit val extendedEd25519Proves: Proves[PrivateKeys.ExtendedEd25519, Proofs.SignatureEd25519] =
      new Proves[PrivateKeys.ExtendedEd25519, Proofs.SignatureEd25519] {
        private val ed = new Ed25519

        def proveWith[Data: Signable](t: PrivateKeys.ExtendedEd25519, data: Data): Proofs.SignatureEd25519 = {
          val signatureArray: Array[Byte] = new Array[Byte](ed.SIGNATURE_SIZE)
          val ctx: Array[Byte] = Array.emptyByteArray
          val phflag: Byte = 0x00
          val h: Array[Byte] = (t.leftKey.data ++ t.rightKey.data).toArray
          val s: Array[Byte] = t.leftKey.data.toArray
          val pk: Array[Byte] = ContainsVerificationKey[PrivateKeys.ExtendedEd25519, PublicKeys.ExtendedEd25519]
            .verificationKeyOf(t)
            .bytes
            .data
            .toArray
          val m: Array[Byte] = data.signableBytes.toArray
          ed.implSign(ed.sha512Digest, h, s, pk, 0, ctx, phflag, m, 0, m.length, signatureArray, 0)
          Proofs.SignatureEd25519(Sized.strictUnsafe(Bytes(signatureArray)))
        }
      }

    implicit val kesPrivateKeyProves: Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] =
      new Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] {

        def proveWith[Data: Signable](t: PrivateKeys.Kes, data: Data): Proofs.Consensus.KesCertificate =
          Proofs.Consensus.KesCertificate(
            Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0))),
            Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))),
            Sized.strictUnsafe(
              Bytes(Array.fill[Byte](implicitly[Proofs.Consensus.KesCertificate.ChainCodeLength].value)(0))
            )
          )
      }
  }
  object instances extends Instances
}
