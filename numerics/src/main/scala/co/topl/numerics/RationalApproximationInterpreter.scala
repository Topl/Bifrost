package co.topl.numerics

import cats.Monad
import cats.implicits._
import co.topl.algebras.RationalApproximation
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

object RationalApproximationInterpreter {

  def make[F[_]: Monad](maxDenominator: BigInt, maxIter: Int): F[RationalApproximation[F]] =
    new RationalApproximation[F] {

      override def rationalApproximation(x: Ratio): F[Ratio] =
        RationalApproximationInterpreter.rationalApproximation(x, maxDenominator, maxIter).pure[F]
    }.pure[F]

  /**
   * Implmentation of Farey approximation technique following this blog post
   * https://www.johndcook.com/blog/2010/10/20/best-rational-approximation/
   * This method reduces the byte size of ratios by finding an
   * approximate answer with a smaller numerator and denominator.
   * Returns a ratio with a denominator no greater than maxDenominator that approximates the input ratio.
   * The approximation gets better with number of iterations.
   * I've modified the algorithm to work with numbers greater than 1 and all negative numbers
   * @param input ratio to be reduced in size
   * @param maxDenominator maximum resultant denominator
   * @param maxIter maximum number of iterations
   * @return a new ratio with smaller integer values that approximates the input
   */
  private[numerics] def rationalApproximation(input: Ratio, maxDenominator: BigInt, maxIter: Int): Ratio = {
    val x =
      if (input.denominator < 0) Ratio(-input.numerator, -input.denominator)
      else input
    var output = input
    val q: BigInt = x.numerator / x.denominator
    val r: BigInt = x.numerator % x.denominator
    val sign = if (x.numerator > 0 && x.denominator > 0) {
      1
    } else {
      -1
    }
    val absx = Ratio(r, x.denominator).abs
    var a = BigInt(0)
    var b = BigInt(1)
    var c = BigInt(1)
    var d = BigInt(1)
    var j = 0
    var not_done = true
    while (b <= maxDenominator && d <= maxDenominator && j <= maxIter && not_done) {
      val med = Ratio(a + c, b + d)
      if (absx == med) {
        if (b + d <= maxDenominator) {
          output = Ratio(sign * (a + c), b + d)
        } else if (d > b) {
          output = Ratio(sign * c, d)
        } else {
          output = Ratio(sign * a, b)
        }
        not_done = false
      } else if (absx > med) {
        a = a + c
        b = b + d
      } else {
        c = a + c
        d = b + d
      }
      j = j + 1
    }
    if (not_done) {
      if (b > maxDenominator) {
        output = Ratio(sign * c, d)
      } else {
        output = Ratio(sign * a, b)
      }
    }

    Ratio(q * output.denominator + output.numerator, output.denominator)
  }

}
