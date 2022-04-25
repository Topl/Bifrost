package co.topl.genus.typeclasses

import cats.data.ValidatedNec
import simulacrum.typeclass

@typeclass trait Validation[T] {
  def validate(value: T): ValidatedNec[String, T]
}
