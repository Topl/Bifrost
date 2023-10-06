package co.topl.crypto.models

case class SignatureKesSum(
  verificationKey: Array[Byte],
  signature:       Array[Byte],
  witness:         Seq[Array[Byte]]
) {

  override def hashCode(): Int = {
    var r = 1
    r = 31 * r + java.util.Arrays.hashCode(verificationKey)
    r = 31 * r + java.util.Arrays.hashCode(signature)
    witness.foreach(w => r = 31 * r + java.util.Arrays.hashCode(w))
    r
  }

  override def equals(other: Any): Boolean = other match {
    case kesSum: SignatureKesSum =>
      verificationKey.sameElements(kesSum.verificationKey) &&
      signature.sameElements(kesSum.signature) &&
      witness.zip(kesSum.witness).forall { case (x, y) => x.sameElements(y) }

    case _ => false
  }
}
