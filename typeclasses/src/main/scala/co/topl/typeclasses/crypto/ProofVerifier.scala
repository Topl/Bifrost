package co.topl.typeclasses.crypto

import co.topl.models._
import co.topl.typeclasses.crypto.Signable.Instances._

import java.nio.charset.StandardCharsets
import scala.language.implicitConversions

trait ProofVerifier[Proof, Proposition] {

  /**
   * Does the given `proof` satisfy the given `proposition` using the given `data`?
   */
  def verifyWith[Data: Signable](proof: Proof, proposition: Proposition, data: Data): Boolean
}

object ProofVerifier {

  trait Ops[Proof, Proposition] {

    def proof: Proof
    def typeclassInstance: ProofVerifier[Proof, Proposition]

    def satisfies[Data: Signable](proposition: Proposition, data: Data): Boolean =
      typeclassInstance.verifyWith(proof, proposition, data)
  }

  trait Implicits {

    implicit def asOps[Proof, Proposition](
      p:                 Proof
    )(implicit verifier: ProofVerifier[Proof, Proposition]): Ops[Proof, Proposition] =
      new Ops[Proof, Proposition] {
        override def proof: Proof = p

        override def typeclassInstance: ProofVerifier[Proof, Proposition] = verifier
      }
  }

  object ops extends Implicits

  object Instances {
    implicit val publicKeyCurve25519: ProofVerifier[Proofs.SignatureCurve25519, Propositions.PublicKeyCurve25519] = ???
    implicit val publicKeyEd25519: ProofVerifier[Proofs.SignatureEd25519, Propositions.PublicKeyEd25519] = ???

    implicit val thresholdCurve25519
      : ProofVerifier[Proofs.ThresholdSignatureCurve25519, Propositions.ThresholdCurve25519] = ???
    implicit val thresholdEd25519: ProofVerifier[Proofs.ThresholdSignatureEd25519, Propositions.ThresholdEd25519] = ???
    implicit val existence: ProofVerifier[Proofs.Existence, Propositions.Existence] = ???
    implicit val consensusVrfTest: ProofVerifier[Proofs.Consensus.VrfTest, Propositions.Consensus.PublicKeyVrf] = ???
    implicit val consensusVrfNonce: ProofVerifier[Proofs.Consensus.Nonce, Propositions.Consensus.PublicKeyVrf] = ???

    implicit val consensusKesCertificate
      : ProofVerifier[Proofs.Consensus.KesCertificate, Propositions.Consensus.PublicKeyKes] =
      ???
    implicit val consensusMMM: ProofVerifier[Proofs.Consensus.MMM, Propositions.Consensus.PublicKeyKes] = ???
  }
}

trait CertificateVerifier[T] {
  def verify(t: T): Boolean
}

object CertificateVerifier {

  object Instances {

    import ProofVerifier.Instances._
    import ProofVerifier.ops._

    implicit def vrfCertificateVerifier(epochNonce: Nonce): CertificateVerifier[VrfCertificate] =
      certificate =>
        certificate.testProof.satisfies(
          Propositions.Consensus.PublicKeyVrf(certificate.vkVRF),
          "test".getBytes(StandardCharsets.UTF_8) ++ epochNonce.toArray
        ) && certificate.testProof.satisfies(
          Propositions.Consensus.PublicKeyVrf(certificate.vkVRF),
          "nonce".getBytes(StandardCharsets.UTF_8) ++ epochNonce.toArray
        )

    implicit def kesCertificateVerifier(blockHeaderV2: BlockHeaderV2): CertificateVerifier[KesCertificate] =
      certificate =>
        certificate.mmmProof.satisfies(Propositions.Consensus.PublicKeyKes(certificate.vkKES), blockHeaderV2) &&
        certificate.kesProof.satisfies(
          Propositions.Consensus.PublicKeyKes(certificate.vkKES),
          // TODO: certificate.vkKES.bytes incorrect here
          certificate.vkKES.bytes.data.toArray ++ BigInt(certificate.slotOffset).toByteArray
        )
  }
}
