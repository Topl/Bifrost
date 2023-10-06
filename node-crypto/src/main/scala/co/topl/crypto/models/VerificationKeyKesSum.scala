package co.topl.crypto.models

case class VerificationKeyKesSum(value: Array[Byte], step: Int) {

  override def hashCode(): Int = {
    var r = 1
    r = 31 * r + java.util.Arrays.hashCode(value) + step.hashCode
    r = 31 * r + step.hashCode
    r
  }

  override def equals(other: Any): Boolean = other match {
    case vk: VerificationKeyKesSum =>
      value.sameElements(vk.value) &&
      step == vk.step
    case _ => false
  }
}
