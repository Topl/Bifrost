package co.topl.stakeholder.primitives

import scala.math.BigInt
import java.math.BigInteger
import scala.annotation.tailrec
import scala.language.implicitConversions

/**
 * Ratio class for handling stake distribution and threshold quantities that range from 0.0 to 1.0
 *
 * Copied from:
 * https://gist.github.com/martintrojer/5845678
 *
 * Credit to Martin Trojer, http://martintrojer.github.io/
 *
 * AMS 2020: Modification made for consensus routines, added comparison operators and some QOL tweaks for ease of use
 *
 * @param n  BigInt numerator
 * @param d  BigInt denominator
 */

case class Ratio(n: BigInt, d: BigInt) {

  private val g = gcd(n.abs, d.abs)
  val numer: BigInt = n / g
  val denom: BigInt = d / g
  require(denom != 0)

  @tailrec
  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)

  val bigdecValue: BigDecimal = BigDecimal(numer) / BigDecimal(denom)

  // ---

  def pow(n: Int) =
    new Ratio(
      this.numer.pow(n),
      this.denom.pow(n)
    )

  def - =
    new Ratio(
      -numer,
      denom
    )

  def *(that: Int) =
    new Ratio(
      numer * that,
      denom
    )

  def /(that: Int) =
    new Ratio(
      numer,
      denom * that
    )

  def *(that: Long) =
    new Ratio(
      numer * that,
      denom
    )

  def /(that: Long) =
    new Ratio(
      numer,
      denom * that
    )

  def +(that: Ratio): Ratio =
    new Ratio(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def -(that: Ratio): Ratio =
    new Ratio(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )

  def *(that: Ratio): Ratio =
    new Ratio(numer * that.numer, denom * that.denom)

  def /(that: Ratio): Ratio =
    new Ratio(numer * that.denom, denom * that.numer)

  def <(that: Ratio): Boolean =
    that.denom * numer < that.numer * denom

  def >(that: Ratio): Boolean =
    that.denom * numer > that.numer * denom

  def <=(that: Ratio): Boolean =
    that.denom * numer <= that.numer * denom

  def >=(that: Ratio): Boolean =
    that.denom * numer >= that.numer * denom

  def toBigDecimal: BigDecimal =
    BigDecimal(numer) / BigDecimal(denom)

  def round: BigInt =
    if (numer.abs > denom.abs) {
      numer.abs / denom.abs
    } else {
      BigInt(1)
    }

  // ---

  override def toString: String = numer.toString() + (if (denom != 1) "/" + denom.toString() else "")

  def toDouble: Double = this.toBigDecimal.toDouble

  override def equals(that: Any): Boolean = that match {
    case that: Ratio => numer == that.numer && denom == that.denom
    case _           => false
  }

  override val hashCode: Int = 41 * numer.hashCode() + denom.hashCode()
}

object Ratio {
  def apply(n: BigInt) = new Ratio(n, 1)
  def apply(n: BigInt, d: BigInt) = new Ratio(n, d)
  def apply(i: Int): Ratio = new Ratio(i, 1)
  def apply(n: Int, d:    Int): Ratio = new Ratio(n, d)

  type BigDec = java.math.BigDecimal

  def apply(b: BigDec): Ratio = {

    val bv = b.unscaledValue()
    val scale = b.scale()
    if (scale < 0)
      new Ratio(bv.multiply(BigInteger.TEN.pow(-scale)), BigInteger.ONE);
    else
      new Ratio(bv, BigInteger.TEN.pow(scale));
  }

  def apply(double: Double, prec: Int): Ratio = {
    val d = BigInt(10).pow(prec)
    val n = (BigDecimal(double).setScale(prec, BigDecimal.RoundingMode.DOWN) * BigDecimal(d)).toBigInt
    new Ratio(n, d)
  }

  implicit object RatioIsFractionalAndOrdered extends RatioIsFractional with RatioOrdering {
    override def parseString(str: String): Option[Ratio] = None
  }

  implicit def intToRatio(i:    Int): Ratio = apply(i)
  implicit def doubleToRatio(d: Double, p: Int): Ratio = apply(d, p)
}

trait RatioIsFractional extends Fractional[Ratio] {
  def plus(x:     Ratio, y: Ratio): Ratio = x + y
  def minus(x:    Ratio, y: Ratio): Ratio = x - y
  def times(x:    Ratio, y: Ratio): Ratio = x * y
  def div(x:      Ratio, y: Ratio): Ratio = x / y
  def negate(x:   Ratio): Ratio = -x
  def fromInt(x:  Int): Ratio = Ratio(x)
  def toInt(x:    Ratio): Int = x.bigdecValue.toInt
  def toLong(x:   Ratio): Long = x.bigdecValue.toLong
  def toFloat(x:  Ratio): Float = x.bigdecValue.toFloat
  def toDouble(x: Ratio): Double = x.bigdecValue.toDouble
}

trait RatioOrdering extends Ordering[Ratio] {
  def compare(a: Ratio, b: Ratio): Int = a.bigdecValue compare b.bigdecValue
}
