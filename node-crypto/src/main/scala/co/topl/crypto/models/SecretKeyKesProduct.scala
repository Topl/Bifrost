package co.topl.crypto.models

case class SecretKeyKesProduct(
  superTree:    KesBinaryTree, // Hour hand
  subTree:      KesBinaryTree, // Minute hand
  nextSubSeed:  Array[Byte],
  subSignature: SignatureKesSum,
  offset:       Long
) {

  override def equals(obj: Any): Boolean =
    obj match {
      case s: SecretKeyKesProduct =>
        superTree == s.superTree &&
        subTree == s.subTree &&
        java.util.Arrays.equals(nextSubSeed, s.nextSubSeed) &&
        subSignature == s.subSignature &&
        offset == s.offset
      case _ =>
        false
    }

  override def hashCode(): Int = {
    var r = 1

    r = 31 * r + superTree.hashCode()
    r = 31 * r + subTree.hashCode()
    r = 31 * r + java.util.Arrays.hashCode(nextSubSeed)
    r = 31 * r + subSignature.hashCode()
    r = 31 * r + offset.hashCode()
    r
  }
}
