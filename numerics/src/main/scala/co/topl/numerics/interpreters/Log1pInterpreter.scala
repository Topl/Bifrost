package co.topl.numerics.interpreters

import cats.Monad
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.models.utility._
import co.topl.numerics.algebras.Log1p
import co.topl.numerics.implicits._
import scalacache.caffeine.CaffeineCache

object Log1pInterpreter extends LentzMethod {

  def make[F[_]: Monad](max_iterations: Int, precision: Int): F[Log1p[F]] = new Log1p[F] {
    override def evaluate(x: Ratio): F[Ratio] = Log1pInterpreter.log1p(x, max_iterations, precision)._1.pure[F]
  }.pure[F]

  def makeCached[F[_]: Sync](log1p: Log1p[F]): F[Log1p[F]] =
    CaffeineCache[F, Ratio, Ratio].map(cache =>
      (x: Ratio) => cache.cachingF(x)(ttl = None)(Sync[F].defer(log1p.evaluate(x)))
    )

  /**
   * Returns the natural logarithm of the argument plus one
   * @param x argument
   * @param maxIter maximum number of iterations
   * @param prec desired precision
   * @return log_e(1+x)
   */
  private[numerics] def log1p(x: Ratio, maxIter: Int, prec: Int): (Ratio, Boolean, Int) = {
    def a(j: Int): Ratio = j match {
      case 0 => Ratio.Zero
      case 1 => x
      case _ => Ratio(j - 1) * Ratio(j - 1) * x
    }
    def b(j: Int): Ratio = j match {
      case 0 => Ratio.Zero
      case 1 => Ratio.One
      case _ => Ratio(j) - Ratio(j - 1) * x
    }
    modified_lentz_method(maxIter, prec, a, b)
  }

}
