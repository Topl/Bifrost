package co.topl.consensus

import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.keys.{ProductPrivateKey, SymmetricKey}
import co.topl.models._
import co.topl.typeclasses.crypto.ContainsVerificationKey.instances._
import co.topl.typeclasses.crypto.Proves.instances._
import co.topl.typeclasses.crypto.Signable.ops._
import co.topl.typeclasses.crypto.{ContainsVerificationKey, Proves, Signable}
import com.google.common.primitives.Ints
import simulacrum.{op, typeclass}
import Signable.instances._

@typeclass trait KesCertifies[T] {
  @op("certify") def certifyWith(t: T, unsignedBlock: BlockHeaderV2.Unsigned): KesCertificate
}

object KesCertifies {

  trait Instances {

    implicit val kesSkCertifies: KesCertifies[PrivateKeys.Kes] =
      new KesCertifies[PrivateKeys.Kes] {
        private val scheme = new KeyEvolvingSignatureScheme

        def certifyWith(t: PrivateKeys.Kes, data: BlockHeaderV2.Unsigned): KesCertificate = {
          val currentSymmetricKey =
            SymmetricKey(
              ProductPrivateKey.deserializeProductKey(
                Ints.toByteArray(
                  t.bytes.data.toArray.length
                ) ++ t.bytes.data.toArray
              )
            )

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
              Bytes(sig.pki.bytes),
              sig.offset,
              Bytes(sig.pkl.bytes)
            )
          }
          KesCertificate(publicKey, kesProof, mmmProof)
        }
      }
  }

  object instances extends Instances
}
