package co.topl.crypto.models

import java.util.Arrays

case class SignatureKesProduct(
  superSignature: SignatureKesSum,
  subSignature:   SignatureKesSum,
  subRoot:        Array[Byte]
) {

  override def hashCode(): Int =
    superSignature.hashCode() +
    subSignature.hashCode() +
    Arrays.hashCode(subRoot)

  override def equals(other: Any): Boolean = other match {
    case kesProduct: SignatureKesProduct =>
      superSignature == kesProduct.superSignature &&
      subSignature == kesProduct.subSignature &&
      subRoot.sameElements(kesProduct.subRoot)

    case _ => false
  }
}
