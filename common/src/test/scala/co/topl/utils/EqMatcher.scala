package co.topl.utils

import cats.{Eq, Show}
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}

trait EqMatcher extends Matchers {

  def eqShow[T: Eq: Show](expected: T): Matcher[T] =
    (left: T) =>
      MatchResult(
        Eq[T].eqv(left, expected),
        s"${Show[T].show(left)} was not equal to ${Show[T].show(expected)})",
        s"${Show[T].show(left)} was equal to ${Show[T].show(expected)} but should not have been"
      )
}
