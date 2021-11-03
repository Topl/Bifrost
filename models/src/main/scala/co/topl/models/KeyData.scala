package co.topl.models

import co.topl.models.utility.BinaryTree

case class KeyData(
  superScheme:        BinaryTree[Array[Byte]],
  subScheme:          BinaryTree[Array[Byte]],
  subSchemeSignature: Bytes,
  subSchemePublicKey: Bytes,
  subSchemeSeed:      Bytes,
  offset:             Long
)
