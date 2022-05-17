package co.topl.algebras

import cats._
import cats.implicits._
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import ClockAlgebra.implicits._

class ClockAlgebraOpsSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  type F[A] = Id[A]

  behavior of "ClockAlgebra.ClockOps"

  it should "calculate the epoch of a slot" in {
    val clock = mock[ClockAlgebra[F]]
    (() => clock.slotsPerEpoch)
      .expects()
      .once()
      .returning(500L.pure[F])

    clock.epochOf(500L) shouldBe 2L
  }

  it should "calculate the epoch boundary" in {
    val clock = mock[ClockAlgebra[F]]
    (() => clock.slotsPerEpoch)
      .expects()
      .once()
      .returning(500L.pure[F])

    val boundary = clock.epochRange(3)

    boundary.start shouldBe 1500L
    boundary.end shouldBe 1999L
  }
}
