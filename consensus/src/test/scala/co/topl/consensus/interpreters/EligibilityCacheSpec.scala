package co.topl.consensus.interpreters

import cats.effect.IO
import cats.effect.implicits._
import cats.implicits._
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalamock.munit.AsyncMockFactory
import co.topl.models.ModelGenerators._
import co.topl.models.generators.consensus.ModelGenerators._
import co.topl.models.utility.Lengths

import co.topl.codecs.bytes.tetra.instances._

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

  test("EligibilityCache.Repopulate") {
    val baseHeader = arbitraryHeader.arbitrary.first
    val h1 =
      baseHeader
        .update(_.height := 1L)
        .update(_.slot := 0L)
        .update(_.eligibilityCertificate.vrfVK := generateVrfVK())
    val h1Id = h1.id
    val h2 =
      baseHeader
        .update(_.parentHeaderId := h1Id)
        .update(_.height := 2L)
        .update(_.slot := 10L)
        .update(_.eligibilityCertificate.vrfVK := generateVrfVK())
    val h2Id = h2.id
    val h3 =
      baseHeader
        .update(_.parentHeaderId := h2Id)
        .update(_.height := 3L)
        .update(_.slot := 15L)
        .update(_.eligibilityCertificate.vrfVK := generateVrfVK())
    val h3Id = h3.id
    val h4 =
      baseHeader
        .update(_.parentHeaderId := h3Id)
        .update(_.height := 4L)
        .update(_.slot := 20L)
        .update(_.eligibilityCertificate.vrfVK := generateVrfVK())
    val h4Id = h4.id
    val headers = Map(h1Id -> h1, h2Id -> h2, h3Id -> h3, h4Id -> h4)
    val testResource =
      for {
        underTest <- EligibilityCache.make[F](10)
        _         <- EligibilityCache.repopulate[F](underTest, 10, h4, headers(_).pure[F]).toResource
        _         <- underTest.tryInclude(h1.eligibilityCertificate.vrfVK, h1.slot).map(!_).assert.toResource
        _         <- underTest.tryInclude(h2.eligibilityCertificate.vrfVK, h2.slot).map(!_).assert.toResource
        _         <- underTest.tryInclude(h3.eligibilityCertificate.vrfVK, h3.slot).map(!_).assert.toResource
        _         <- underTest.tryInclude(h4.eligibilityCertificate.vrfVK, h4.slot).map(!_).assert.toResource
      } yield ()

    testResource.use_
  }

  private def generateVrfVK() = genSizedStrictBytes[Lengths.`32`.type]().first.data
}
