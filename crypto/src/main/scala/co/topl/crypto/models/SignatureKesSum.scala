package co.topl.crypto.models

case class SignatureKesSum(
  verificationKey: Array[Byte],
  signature:       Array[Byte],
  witness:         Seq[Array[Byte]]
)
