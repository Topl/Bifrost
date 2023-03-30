package co.topl.consensus.interpreters

import cats.effect.IO
import cats.effect.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalamock.munit.AsyncMockFactory
import co.topl.models.ModelGenerators._
import co.topl.models.utility.Lengths

class EligibilityCacheSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  test("Eligibility Cache with new entry") {
    val entry1 = (generateVrfVK(), 1L)

    val resource =
      for {
        underTest <- EligibilityCache.make[F](10)
        _         <- (underTest.tryInclude _).tupled(entry1).assert.toResource
      } yield ()

    resource.use_
  }

  test("Eligibility Cache with old entry") {
    val entry1 = (generateVrfVK(), 1L)
    val entry2 = (generateVrfVK(), 2L)

    val resource =
      for {
        underTest <- EligibilityCache.make[F](10)
        _         <- (underTest.tryInclude _).tupled(entry1).assert.toResource
        _         <- (underTest.tryInclude _).tupled(entry1).map(!_).assert.toResource
        _         <- (underTest.tryInclude _).tupled(entry2).assert.toResource
      } yield ()

    resource.use_
  }

  test("Eligibility Cache with new entry and overflow") {
    val entry1 = (generateVrfVK(), 1L)
    val entry2 = (generateVrfVK(), 2L)
    val entry3 = (generateVrfVK(), 3L)
    val entry3A = (generateVrfVK(), 3L)

    val resource =
      for {
        underTest <- EligibilityCache.make[F](2)
        _         <- (underTest.tryInclude _).tupled(entry1).assert.toResource
        _         <- (underTest.tryInclude _).tupled(entry2).assert.toResource
        _         <- (underTest.tryInclude _).tupled(entry3).assert.toResource
        // NOTE: No way to assert that entry1 was removed.  Attempting to `tryInclude` it again would return "true",
        // but the entry would be immediately removed because it's at an "older" slot
        _ <- (underTest.tryInclude _).tupled(entry2).map(!_).assert.toResource
        _ <- (underTest.tryInclude _).tupled(entry3).map(!_).assert.toResource
        _ <- (underTest.tryInclude _).tupled(entry3A).assert.toResource
      } yield ()

    resource.use_
  }

  private def generateVrfVK() = genSizedStrictBytes[Lengths.`32`.type]().first.data
}
