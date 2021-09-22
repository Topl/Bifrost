package co.topl.models

case class KeyData(
  superScheme:        Tree[Array[Byte]],
  subScheme:          Tree[Array[Byte]],
  subSchemeSignature: Bytes,
  subSchemePublicKey: Bytes,
  subSchemeSeed:      Bytes,
  offset:             Long
)
