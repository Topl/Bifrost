package co.topl.typeclasses.crypto

import co.topl.models._
import simulacrum.{op, typeclass}

/**
 * Public-Private Key Verifier
 */
@typeclass trait ProofVerifier[T, P <: Proof] {
  @op("verify") def verifyWith[Data: Signable](t: T, data: Data, proof: P): Boolean
}

object ProofVerifier {

  object Instances {
    implicit val publicKeyCurve25519: ProofVerifier[PublicKeys.Curve25519, Proofs.SignatureCurve25519] = ???
    implicit val publicKeyEd25519: ProofVerifier[PublicKeys.Ed25519, Proofs.SignatureEd25519] = ???
    implicit val publicKeyVrf: ProofVerifier[PublicKeys.Vrf] = ???
    implicit val publicKeyKes: ProofVerifier[PublicKeys.Kes] = ???
  }
}

trait CertificateVerifier[T] {
  def verify(t: T): Boolean
}

object CertificateVerifier {

  object Instances {

    implicit val vrfCertificateVerifier: CertificateVerifier[VrfCertificate] =
      (certificate) =>
        // 1. verify(certificate.vkVRF, "test" + epochNonce, testProof) &&
        //    verify(certificate.vkVRF, "nonce" + epochNonce, nonceProof)
        ???

    implicit val kesCertificateVerifier: CertificateVerifier[KesCertificate] =
      certificate =>
        // 1. verify(certificate.vkKES, blockHeader.messageToSign, certificate.blockProof) &&
        //    verify(
        //      blockHeader.address.stakingVerificationKey,
        //      certificate.vkKES ++ bytesOf(certificate.slotOffset,
        //      certificate.kesProof
        //    )
        ???
  }
}
