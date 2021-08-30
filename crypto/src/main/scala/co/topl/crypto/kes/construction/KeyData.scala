package co.topl.crypto.kes.construction

case class KeyData(
  superScheme:        Tree[Array[Byte]],
  subScheme:          Tree[Array[Byte]],
  subSchemeSignature: Array[Byte],
  subSchemePublicKey: Array[Byte],
  subSchemeSeed:      Array[Byte],
  offset:             Long
)
