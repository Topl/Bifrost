package co.topl.crypto.models

case class SignatureKesProduct(
  superSignature: SignatureKesSum,
  subSignature:   SignatureKesSum,
  subRoot:        Array[Byte]
) {

  override def hashCode(): Int = {
    var r = 1
    r = 31 * r + superSignature.hashCode()
    r = 31 * r + subSignature.hashCode()
    r = 31 * r + java.util.Arrays.hashCode(subRoot)
    r
  }

  override def equals(other: Any): Boolean = other match {
    case kesProduct: SignatureKesProduct =>
      superSignature == kesProduct.superSignature &&
      subSignature == kesProduct.subSignature &&
      subRoot.sameElements(kesProduct.subRoot)

    case _ => false
  }
}
