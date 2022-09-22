package co.topl.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import munit.CatsEffectSuite
import org.scalamock.munit.AsyncMockFactory

class CacheStoreSpec extends CatsEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Read, store, and delete cache values") {
    withMock {
      for {
        underlying <- mock[Store[F, Long, String]].pure[F]
        underTest <- CacheStore.make[F, Long, java.lang.Long, String](
          underlying.pure[F],
          Long.box,
          identity,
          None
        )
        _ = (underlying.get(_)).expects(5L).once().returning(none[String].pure[F])
        _ <- underTest.get(5L).assertEquals(None)
        _ = (underlying.put(_, _)).expects(6L, "Test").once().returning(().pure[F])
        _ <- underTest.put(6L, "Test")
        _ = (underlying.get(_)).expects(6L).never()
        _ <- underTest.get(6L).assertEquals("Test".some)
        _ = (underlying.get(_)).expects(7L).once().returning("Test2".some.pure[F])
        _ <- underTest.get(7L).assertEquals("Test2".some)
        _ = (underlying.remove(_)).expects(7L).once().returning(().pure[F])
        _ <- underTest.remove(7L)
      } yield ()
    }
  }
}
