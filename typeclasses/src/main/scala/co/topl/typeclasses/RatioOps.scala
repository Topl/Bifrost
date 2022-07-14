package co.topl.typeclasses

import co.topl.models.utility.Ratio

import scala.language.implicitConversions
import scala.math.BigInt

object RatioOps {

  trait Implicits {

    implicit class Ops(ratio: Ratio) {

      def inverse: Ratio =
        Ratio(ratio.denominator, ratio.numerator)

      def abs: Ratio =
        Ratio(
          ratio.numerator match {
            case num if num < 0 => -num
            case num            => num
          },
          ratio.denominator match {
            case den if den < 0 => -den
            case den            => den
          }
        )

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

      def toDouble: Double = this.toBigDecimal.toDouble

      def round: BigInt =
        if (ratio.numerator.abs > ratio.denominator.abs) {
          ratio.numerator.abs / ratio.denominator.abs
        } else {
          BigInt(1)
        }
    }
  }

  object implicits extends Implicits
}
