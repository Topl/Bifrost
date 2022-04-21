package co.topl.numerics

import cats.Monad
import cats.implicits._
import co.topl.algebras.Exp
import co.topl.models.utility._
import co.topl.typeclasses.implicits._

object ExpInterpreter extends LentzMethod {

  def make[F[_]: Monad](max_iterations: Int, precision: Int): F[Exp[F]] = new Exp[F] {
    override def evaluate(x: Ratio): F[Ratio] = ExpInterpreter.exp(x, max_iterations, precision)._1.pure[F]
  }.pure[F]

  /**
   * Returns the exponent base natural number of the argument
   * @param x argument
   * @param maxIter maximum number of iterations
   * @param prec desired precision
   * @return exp(x)
   */

  private[numerics] def exp(x: Ratio, maxIter: Int, prec: Int): (Ratio, Boolean, Int) = {
    def a(j: Int): Ratio = j match {
      case 0 => Ratio.Zero
      case 1 => Ratio.One
      case 2 => Ratio.NegativeOne * x
      case _ => Ratio(-j + 2) * x
    }
    def b(j: Int): Ratio = j match {
      case 0 => Ratio.Zero
      case 1 => Ratio.One
      case _ => Ratio(j - 1) + x
    }
    if (x == Ratio.Zero) (Ratio.One, true, 0)
    else modified_lentz_method(maxIter, prec, a, b)
  }

}
