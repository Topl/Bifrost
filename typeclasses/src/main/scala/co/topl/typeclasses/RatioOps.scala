package co.topl.typeclasses

import co.topl.models.Ratio

import scala.math.BigInt

object RatioOps {

  trait Ops {
    def ratio: Ratio

    val bigdecValue: BigDecimal = BigDecimal(ratio.numererator) / BigDecimal(ratio.denominator)

    def pow(n: Int) =
      Ratio(
        ratio.numererator.pow(n),
        ratio.denominator.pow(n)
      )

    def - =
      Ratio(
        -ratio.numererator,
        ratio.denominator
      )

    def *(that: Int) =
      Ratio(
        ratio.numererator * that,
        ratio.denominator
      )

    def /(that: Int) =
      Ratio(
        ratio.numererator,
        ratio.denominator * that
      )

    def *(that: Long) =
      Ratio(
        ratio.numererator * that,
        ratio.denominator
      )

    def /(that: Long) =
      Ratio(
        ratio.numererator,
        ratio.denominator * that
      )

    def +(that: Ratio): Ratio =
      Ratio(
        ratio.numererator * that.denominator + that.numererator * ratio.denominator,
        ratio.denominator * that.denominator
      )

    def -(that: Ratio): Ratio =
      Ratio(
        ratio.numererator * that.denominator - that.numererator * ratio.denominator,
        ratio.denominator * that.denominator
      )

    def *(that: Ratio): Ratio =
      Ratio(ratio.numererator * that.numererator, ratio.denominator * that.denominator)

    def /(that: Ratio): Ratio =
      Ratio(ratio.numererator * that.denominator, ratio.denominator * that.numererator)

    def <(that: Ratio): Boolean =
      that.denominator * ratio.numererator < that.numererator * ratio.denominator

    def >(that: Ratio): Boolean =
      that.denominator * ratio.numererator > that.numererator * ratio.denominator

    def <=(that: Ratio): Boolean =
      that.denominator * ratio.numererator <= that.numererator * ratio.denominator

    def >=(that: Ratio): Boolean =
      that.denominator * ratio.numererator >= that.numererator * ratio.denominator

    def toBigDecimal: BigDecimal =
      BigDecimal(ratio.numererator) / BigDecimal(ratio.denominator)

    def round: BigInt =
      if (ratio.numererator.abs > ratio.denominator.abs) {
        ratio.numererator.abs / ratio.denominator.abs
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
