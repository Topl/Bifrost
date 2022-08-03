package co.topl.algebras

import co.topl.models.utility.Ratio

trait RationalApproximation[F[_]] {
  def rationalApproximation(x: Ratio): F[Ratio]
}
