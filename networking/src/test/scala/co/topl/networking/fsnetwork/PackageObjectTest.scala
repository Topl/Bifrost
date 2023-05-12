package co.topl.networking.fsnetwork

import cats.{MonadThrow, Show}
import cats.effect.IO
import cats.implicits._
import co.topl.algebras.Store
import co.topl.consensus.models.{BlockId, SlotData}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.networking.fsnetwork.BlockCheckerTest.F
import co.topl.networking.fsnetwork.TestHelper.CallHandler3Ops
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.typeclasses.implicits._

object PackageObjectTest {
  type F[A] = IO[A]
}

class PackageObjectTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)

  test("getFromChainUntil shall work properly") {
    withMock {
      val blockIdSlotId =
        TestHelper
          .arbitraryLinkedSlotDataHeaderBlockNoTx(Gen.choose[Long](0, 10))
          .arbitrary
          .first
          .map(d => (d._1, d._2))
      val testDataSize = blockIdSlotId.length

      val storageData = blockIdSlotId.toList.toMap
      val storage = mock[Store[F, BlockId, SlotData]]
      (storage
        .getOrRaise(_: BlockId)(_: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .rep(testDataSize.toInt)
        .onCall { case (id: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          storageData(id).pure[F]
        }

      for {
        data <- getFromChainUntil[F, BlockId](
          getSlotDataFromT = storage.getOrRaise,
          getT = id => id.pure[F],
          terminateOn = id => (!storageData.contains(id)).pure[F]
        )(blockIdSlotId.last._1)
        _ = assert(data == blockIdSlotId.map(_._1).toList)

      } yield ()
    }
  }

  test("getFromChainUntil shall work properly in case of monad throw error") {
    withMock {
      val blockIdSlotId =
        TestHelper
          .arbitraryLinkedSlotDataHeaderBlockNoTx(Gen.choose[Long](0, 10))
          .arbitrary
          .first
          .map(d => (d._1, d._2))

      val storageData = blockIdSlotId.toList.toMap
      val storage = mock[Store[F, BlockId, SlotData]]
      (storage
        .getOrRaise(_: BlockId)(_: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked))
        .expects(*, *, *)
        .once()
        .onCall { case (_: BlockId, _: MonadThrow[F] @unchecked, _: Show[BlockId] @unchecked) =>
          val monadThrow = implicitly[MonadThrow[F]]
          monadThrow.raiseError(new IllegalStateException())
        }

      for {
        data <- getFromChainUntil[F, BlockId](
          getSlotDataFromT = storage.getOrRaise,
          getT = id => id.pure[F],
          terminateOn = id => (!storageData.contains(id)).pure[F]
        )(blockIdSlotId.last._1).handleError(_ => List.empty[BlockId])
        _ = assert(data == List.empty[BlockId])

      } yield ()
    }
  }

}
