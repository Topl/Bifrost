package co.topl.typeclasses

import co.topl.models.utility.Ratio

import scala.language.implicitConversions
import scala.math.BigInt

object RatioOps {

  trait Ops {
    def ratio: Ratio

    val bigdecValue: BigDecimal = BigDecimal(ratio.numerator) / BigDecimal(ratio.denominator)

    def pow(n: Int) =
      Ratio(
        ratio.numerator.pow(n),
        ratio.denominator.pow(n)
      )

    def - =
      Ratio(
        -ratio.numerator,
        ratio.denominator
      )

    def *(that: Int) =
      Ratio(
        ratio.numerator * that,
        ratio.denominator
      )

    def /(that: Int) =
      Ratio(
        ratio.numerator,
        ratio.denominator * that
      )

    def *(that: Long) =
      Ratio(
        ratio.numerator * that,
        ratio.denominator
      )

    def /(that: Long) =
      Ratio(
        ratio.numerator,
        ratio.denominator * that
      )

    def +(that: Ratio): Ratio =
      Ratio(
        ratio.numerator * that.denominator + that.numerator * ratio.denominator,
        ratio.denominator * that.denominator
      )

    def -(that: Ratio): Ratio =
      Ratio(
        ratio.numerator * that.denominator - that.numerator * ratio.denominator,
        ratio.denominator * that.denominator
      )

    def *(that: Ratio): Ratio =
      Ratio(ratio.numerator * that.numerator, ratio.denominator * that.denominator)

    def /(that: Ratio): Ratio =
      Ratio(ratio.numerator * that.denominator, ratio.denominator * that.numerator)

    def <(that: Ratio): Boolean =
      that.denominator * ratio.numerator < that.numerator * ratio.denominator

    def >(that: Ratio): Boolean =
      that.denominator * ratio.numerator > that.numerator * ratio.denominator

    def <=(that: Ratio): Boolean =
      that.denominator * ratio.numerator <= that.numerator * ratio.denominator

    def >=(that: Ratio): Boolean =
      that.denominator * ratio.numerator >= that.numerator * ratio.denominator

    def toBigDecimal: BigDecimal =
      BigDecimal(ratio.numerator) / BigDecimal(ratio.denominator)

    def round: BigInt =
      if (ratio.numerator.abs > ratio.denominator.abs) {
        ratio.numerator.abs / ratio.denominator.abs
      } else {
        BigInt(1)
      }
  }

  trait ToOps {

    implicit def toRatioOps(r: Ratio): Ops =
      new Ops {
        override def ratio: Ratio = r
      }
  }

  object implicits extends ToOps
}
