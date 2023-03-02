package co.topl.crypto.models

case class SignatureKesProduct(
  superSignature: SignatureKesSum,
  subSignature:   SignatureKesSum,
  subRoot:        Array[Byte]
)
