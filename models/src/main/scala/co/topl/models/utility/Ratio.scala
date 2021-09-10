package co.topl.models.utility

import scala.annotation.tailrec

case class Ratio(numerator: BigInt, denominator: BigInt, greatestCommonDenominator: BigInt) {

  override def toString(): String =
    numerator.toString + (if (denominator != 1) ("/" + denominator) else "")

  override def equals(that: Any): Boolean =
    that match {
      case that: Ratio => numerator == that.numerator && denominator == that.denominator
      case _           => false
    }

  override def hashCode: Int =
    41 * numerator.hashCode() + denominator.hashCode()
}

object Ratio {
  def apply(n: BigInt): Ratio = apply(n, 1: BigInt)

  def apply(n: BigInt, d: BigInt): Ratio = {
    val gcdVal = gcd(n, d)
    Ratio(n / gcdVal, d / gcdVal, gcdVal)
  }

  def apply(i: Int): Ratio = Ratio(i: BigInt, 1: BigInt)
  def apply(n: Int, d: Int): Ratio = apply(n: BigInt, d: BigInt)

  def apply(double: Double, prec: Int): Ratio = {
    val d = BigInt(10).pow(prec)
    val n = (BigDecimal(double).setScale(prec, BigDecimal.RoundingMode.DOWN) * BigDecimal(d)).toBigInt
    new Ratio(n, d, gcd(n, d))
  }

  @tailrec
  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)
}
