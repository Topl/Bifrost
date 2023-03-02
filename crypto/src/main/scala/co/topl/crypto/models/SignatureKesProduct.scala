package co.topl.crypto.models

import com.google.protobuf.ByteString

case class SignatureKesProduct(
  superSignature: SignatureKesSum,
  subSignature:   SignatureKesSum,
  subRoot:        ByteString
)
