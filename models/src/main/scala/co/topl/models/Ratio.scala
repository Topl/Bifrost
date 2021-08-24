package co.topl.models

import scala.annotation.tailrec

case class Ratio(numererator: BigInt, denominator: BigInt, greatestCommonDenominator: BigInt) {

  override def toString =
    numererator + (if (denominator != 1) ("/" + denominator) else "")

  override def equals(that: Any): Boolean =
    that match {
      case that: Ratio => numererator == that.numererator && denominator == that.denominator
      case _           => false
    }

  override val hashCode: Slot =
    41 * numererator.hashCode() + denominator.hashCode()
}

object Ratio {
  def apply(n: BigInt): Ratio = apply(n, 1: BigInt)

  def apply(n: BigInt, d: BigInt): Ratio = {
    val gcdVal = gcd(n, d)
    Ratio(n / gcdVal, d / gcdVal, gcdVal)
  }

  def apply(i: Int): Ratio = Ratio(i: BigInt, 1: BigInt)
  def apply(n: Int, d: Int): Ratio = apply(n: BigInt, d: BigInt)

  @tailrec
  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)
}
