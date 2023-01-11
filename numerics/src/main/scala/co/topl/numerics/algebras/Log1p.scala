package co.topl.numerics.algebras

import co.topl.models.utility.Ratio

trait Log1p[F[_]] {
  def evaluate(x: Ratio): F[Ratio]
}
