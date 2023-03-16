package co.topl.crypto.models

case class SecretKeyKesProduct(
  superTree:    KesBinaryTree, // Hour hand
  subTree:      KesBinaryTree, // Minute hand
  nextSubSeed:  Array[Byte],
  subSignature: SignatureKesSum,
  offset:       Long
)
