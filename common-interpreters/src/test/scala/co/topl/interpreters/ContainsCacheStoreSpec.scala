package co.topl.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import munit.CatsEffectSuite
import org.scalamock.munit.AsyncMockFactory

class ContainsCacheStoreSpec extends CatsEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Read, store, and delete cache values") {
    withMock {
      for {
        underlying <- mock[Store[F, Long, String]].pure[F]
        underTest  <- ContainsCacheStore.make[F, Long, String](underlying.pure[F], 5)

        // check using underlying cache
        _ = (underlying.get _).expects(5L).once().returning(none[String].pure[F])
        _ <- underTest.get(5L).assertEquals(None)

        // put information shall be cached
        _ = (underlying.contains _).expects(6L).never()

        _ = (underlying.put _).expects(6L, "Test").once().returning(().pure[F])
        _ <- underTest.put(6L, "Test")
        _ <- underTest.contains(6L).assertEquals(true)

        // remove information shall be cached
        _ = (underlying.remove _).expects(6L).once().returning(().pure[F])
        _ <- underTest.remove(6L)
        _ <- underTest.contains(6L).assertEquals(false)

        // contains information shall be cached
        _ = (underlying.contains _).expects(7L).once().returning(true.pure[F])
        _ <- underTest.contains(7L).assertEquals(true)
        _ <- underTest.contains(7L).assertEquals(true)
      } yield ()
    }
  }
}
