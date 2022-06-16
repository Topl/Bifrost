package co.topl.algebras

import cats.data.ValidatedNec

trait ContextualValidationAlgebra[F[_], E, T, Context] {

  /**
   * Determines the validity of the given value, within some context.
   * (i.e. if T is a Transaction, there is context about the sequence of transactions leading up to the given `t`)
   */
  def validate(context: Context)(t: T): F[ValidatedNec[E, T]]
}
