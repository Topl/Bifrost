package co.topl.crypto.typeclasses

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.models._
import simulacrum.{op, typeclass}

@typeclass trait KesCertifies[T] {
  @op("certify") def certifyWith(t: T, unsignedBlock: BlockHeaderV2.Unsigned): OperationalCertificate
}

object KesCertifies {

  trait Instances {

    implicit val kesSkCertifies: KesCertifies[SecretKeys.SymmetricMMM] =
      new KesCertifies[SecretKeys.SymmetricMMM] {
        private val scheme = new KeyEvolvingSignatureScheme

        def certifyWith(t: SecretKeys.SymmetricMMM, data: BlockHeaderV2.Unsigned): OperationalCertificate = {
          import ContainsVerificationKey.instances._
          import Signable.instances._
          import Signable.ops._
          val publicKey =
            implicitly[ContainsVerificationKey[SecretKeys.SymmetricMMM, VerificationKeys.Kes]].verificationKeyOf(t)

          val kesProof =
            implicitly[Proves[SecretKeys.SymmetricMMM, Proofs.Signature.Ed25519]]
              .proveWith(t, publicKey)

          val mmmProof = {
            val sig =
              scheme.signSymmetricProduct(
                t,
                data.signableBytes.toArray
              )
            Proofs.Consensus.MMM(
              sig.sigi,
              sig.sigm,
              sig.pki,
              sig.offset,
              sig.pkl
            )
          }
          OperationalCertificate(publicKey, ???, kesProof, mmmProof)
        }
      }
  }

  object instances extends Instances
}
