package co.topl.algebras

import cats.implicits._
import ClockAlgebra.implicits._
import cats.effect.IO
import co.topl.models.{Epoch, Slot}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class ClockAlgebraOpsSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("calculate the epoch of a slot") {
    withMock {
      val clock = mock[ClockAlgebra[F]]
      (() => clock.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(500L.pure[F])

      clock.epochOf(0L).assertEquals(-1L) >>
      clock.epochOf(1L).assertEquals(0L) >>
      clock.epochOf(500L).assertEquals(0L) >>
      clock.epochOf(499L).assertEquals(0L) >>
      clock.epochOf(501L).assertEquals(1L) >>
      clock.epochOf(1000L).assertEquals(1L) >>
      clock.epochOf(1001L).assertEquals(2L) >>
      clock.epochOf(1500L).assertEquals(2L) >>
      clock.epochOf(1500L).assertEquals(2L)
      clock.epochOf(1501L).assertEquals(3L)
    }
  }

  test("calculate the epoch boundary") {
    withMock {
      val clock = mock[ClockAlgebra[F]]
      (() => clock.slotsPerEpoch)
        .expects()
        .anyNumberOfTimes()
        .returning(500L.pure[F])

      def check(epoch: Epoch)(expectedStart: Slot, expectedEnd: Slot) =
        clock
          .epochRange(epoch)
          .flatMap(boundary =>
            boundary.start.pure[F].assertEquals(expectedStart) *> boundary.end.pure[F].assertEquals(expectedEnd)
          )

      check(-2)(-1, -1) *>
      check(-1)(0, 0) *>
      check(0)(1, 500) *>
      check(1)(501, 1000) *>
      check(2)(1001, 1500) *>
      check(3)(1501, 2000)
    }
  }
}
