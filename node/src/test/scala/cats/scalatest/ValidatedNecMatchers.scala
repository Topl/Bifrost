package cats.scalatest

import org.scalatest.matchers.{MatchResult, Matcher}
import cats.data.ValidatedNec

trait ValidatedNecMatchers {
  /**
    * Checks if a `cats.data.ValidatedNec` contains a specific failure element
    * Usage:
    * {{{
    *   validationObj should haveInvalid (someErrorMessageOrObject)
    * }}}
    * Can also be used to test multiple elements: `
    * {{{
    *  validationObj should (haveInvalid (someErrorMessageOrObject) and
    *                       haveInvalid (someOtherErrorMessageOrObject))
    * }}}
    *
    */
  def haveInvalidC[E](element: E): Matcher[ValidatedNec[E, _]] = new HasCatsValidatedNecFailure[E](element)

}

/**
  * Import singleton in case you prefer to import rather than mix in.
  * {{{
  * import ValidatedMatchers._
  * result should beValid (100)
  * }}}
  */
final object ValidatedNecMatchers extends ValidatedNecMatchers

//Classes used above
final private[scalatest] class HasCatsValidatedNecFailure[E](element: E) extends Matcher[ValidatedNec[E, _]] {
  def apply(validated: ValidatedNec[E, _]): MatchResult =
    MatchResult(
      validated.fold(n => (n.head :: n.tail.toList).contains(element), _ => false),
      s"'$validated' did not contain an Invalid element matching '$element'.",
      s"'$validated' contained an Invalid element matching '$element', but should not have."
    )
}