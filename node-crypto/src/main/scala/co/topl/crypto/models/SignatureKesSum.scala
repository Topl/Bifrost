package co.topl.crypto.models

import java.util.Arrays

case class SignatureKesSum(
  verificationKey: Array[Byte],
  signature:       Array[Byte],
  witness:         Seq[Array[Byte]]
) {

  override def hashCode(): Int =
    Arrays.hashCode(verificationKey) +
    Arrays.hashCode(signature) +
    witness.map(Arrays.hashCode).sum

  override def equals(other: Any): Boolean = other match {
    case kesSum: SignatureKesSum =>
      verificationKey.sameElements(kesSum.verificationKey) &&
      signature.sameElements(kesSum.signature) &&
      witness.zip(kesSum.witness).forall { case (x, y) => x.sameElements(y) }

    case _ => false
  }
}
