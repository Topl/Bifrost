package co.topl.crypto.models

import com.google.protobuf.ByteString

case class SignatureKesSum(
  verificationKey: VerificationKeyEd25519,
  signature:       SignatureEd25519,
  witness:         Seq[ByteString]
)
