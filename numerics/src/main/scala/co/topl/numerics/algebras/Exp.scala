package co.topl.numerics.algebras

import co.topl.models.utility.Ratio

trait Exp[F[_]] {
  def evaluate(x: Ratio): F[Ratio]
}
