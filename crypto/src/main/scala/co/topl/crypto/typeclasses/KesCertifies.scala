package co.topl.crypto.typeclasses

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.{ProductPrivateKey, SymmetricKey}
import co.topl.models._
import com.google.common.primitives.Ints
import simulacrum.{op, typeclass}

@typeclass trait KesCertifies[T] {
  @op("certify") def certifyWith(t: T, unsignedBlock: BlockHeaderV2.Unsigned): KesCertificate
}

object KesCertifies {

  trait Instances {

    implicit val kesSkCertifies: KesCertifies[PrivateKeys.Kes] =
      new KesCertifies[PrivateKeys.Kes] {
        private val scheme = new KeyEvolvingSignatureScheme

        def certifyWith(t: PrivateKeys.Kes, data: BlockHeaderV2.Unsigned): KesCertificate = {
          import ContainsVerificationKey.instances._
          import Proves.instances._
          import Signable.instances._
          import Signable.ops._
          val currentSymmetricKey = SymmetricKey.deserializeSymmetricKey(t.bytes.data.toArray)

          val publicKey =
            implicitly[ContainsVerificationKey[PrivateKeys.Kes, PublicKeys.Kes]].verificationKeyOf(t)

          val kesProof =
            implicitly[Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate]]
              .proveWith(t, publicKey)

          val mmmProof = {
            val sig =
              scheme.signSymmetricProduct(
                currentSymmetricKey,
                data.signableBytes.toArray
              )
            Proofs.Consensus.MMM(
              Bytes(sig.sigi),
              Bytes(sig.sigm),
              Bytes(sig.pki.value),
              sig.offset,
              Bytes(sig.pkl.value)
            )
          }
          KesCertificate(publicKey, kesProof, mmmProof)
        }
      }
  }

  object instances extends Instances
}
