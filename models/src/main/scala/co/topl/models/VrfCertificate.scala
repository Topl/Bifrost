package co.topl.models

case class VrfCertificate(
  publicKey: PublicKeys.Ed25519,
  proof:     Sized.Strict[Bytes, Lengths.`64`.type],
  testProof: Sized.Strict[Bytes, Lengths.`80`.type]
)
