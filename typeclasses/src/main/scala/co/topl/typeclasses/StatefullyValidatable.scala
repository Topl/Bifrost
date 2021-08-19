package co.topl.typeclasses

/**
 * Satisfies that T can be validated against some State
 * @tparam State The type of State to validate against
 * @tparam T The value being validated
 * @tparam ValidatedRepr The representation of the value if it passes validation
 * @tparam Failure The failure type if the value fails validation
 */
trait StatefullyValidatable[State, T, ValidatedRepr, Failure] {
  def validate(t: T, state: State): Either[Failure, ValidatedRepr]
}
