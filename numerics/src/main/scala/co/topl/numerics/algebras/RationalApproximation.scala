package co.topl.numerics.algebras

import co.topl.models.utility.Ratio

trait RationalApproximation[F[_]] {
  def rationalApproximation(x: Ratio): F[Ratio]
}
