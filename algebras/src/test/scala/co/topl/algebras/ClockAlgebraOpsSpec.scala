package co.topl.algebras

import cats.implicits._
import ClockAlgebra.implicits._
import cats.effect.IO
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class ClockAlgebraOpsSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("calculate the epoch of a slot") {
    withMock {
      val clock = mock[ClockAlgebra[F]]
      (() => clock.slotsPerEpoch)
        .expects()
        .once()
        .returning(500L.pure[F])

      clock.epochOf(500L).assertEquals(1L)
    }
  }

  test("calculate the epoch boundary") {
    withMock {
      val clock = mock[ClockAlgebra[F]]
      (() => clock.slotsPerEpoch)
        .expects()
        .once()
        .returning(500L.pure[F])

      clock.epochRange(3).map(boundary => boundary.start == 1500L && boundary.end == 1999L).assert
    }
  }
}
