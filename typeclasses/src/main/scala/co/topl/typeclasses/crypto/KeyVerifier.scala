package co.topl.typeclasses.crypto

import co.topl.models.{Bytes, KesCertificate, VrfCertificate}

/**
 * Public-Private Key Verifier
 */
trait KeyVerifier[T, Message, Proof] {
  def verify(t: T, messageToSign: Bytes, proof: Bytes): Boolean
}

object KeyVerifier {}

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
