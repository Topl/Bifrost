package co.topl.crypto.models

import java.util.Arrays

case class VerificationKeyKesProduct(value: Array[Byte], step: Int) {

  override def hashCode(): Int =
    Arrays.hashCode(value) + step.hashCode

  override def equals(other: Any): Boolean = other match {
    case vk: VerificationKeyKesProduct =>
      value.sameElements(vk.value) &&
      step == vk.step
    case _ => false
  }
}
