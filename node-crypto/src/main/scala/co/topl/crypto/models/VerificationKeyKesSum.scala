package co.topl.crypto.models

import java.util.Arrays

case class VerificationKeyKesSum(value: Array[Byte], step: Int) {

  override def hashCode(): Int =
    Arrays.hashCode(value) + step.hashCode

  override def equals(other: Any): Boolean = other match {
    case vk: VerificationKeyKesSum =>
      value.sameElements(vk.value) &&
      step == vk.step
    case _ => false
  }
}
