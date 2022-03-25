package co.topl.algebras

import co.topl.models.utility.Ratio

trait Exp[F[_]] {
  def evaluate(x: Ratio): F[Ratio]
}
