package co.topl.consensus.interpreters

import cats.Applicative
import cats.effect.IO
import cats.implicits._
import co.topl.consensus.algebras.ChainSelectionAlgebra
import co.topl.consensus.models.{BlockId, SlotData, SlotId}
import co.topl.eventtree.EventSourcedState
import co.topl.models.ModelGenerators._
import co.topl.models.generators.common.ModelGenerators.genSizedStrictByteString
import co.topl.models.generators.consensus.ModelGenerators.etaGen
import co.topl.models.utility.Lengths
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory
import co.topl.algebras.Stats.Implicits._

import scala.concurrent.duration._
import cats.effect.kernel.Async
import cats.data.{NonEmptyChain, Validated}

class LocalChainSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  private val blockId0 = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(0)))
  private val blockId1 = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(1)))
  private val blockId2 = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(2)))

  val chainSelection: ChainSelectionAlgebra[F, BlockId, SlotData] =
    new ChainSelectionAlgebra[F, BlockId, SlotData] {

      override def compare(
        x:        SlotData,
        y:        SlotData,
        xFetcher: BlockId => F[SlotData],
        yFetcher: BlockId => F[SlotData]
      ): F[Int] =
        x.height.compareTo(y.height).pure[F]
      override def enoughHeightToCompare(currentHeight: Long, commonHeight: Long, proposedHeight: Long): F[Long] = ???
    }

  val emptyFetcher: BlockId => F[SlotData] = { _: BlockId =>
    SlotData(SlotId.of(1, blockId1), SlotId.of(0, blockId0)).pure[F]
  }

  test("store the head of the local canonical tine") {
    PropF.forAllF(genSizedStrictByteString[Lengths.`64`.type](), etaGen) { (rho, eta) =>
      withMock {
        val initialHead =
          SlotData(
            SlotId.of(1, blockId1),
            SlotId.of(0, blockId0),
            rho.data,
            eta.data,
            0
          )

        val blockHeights =
          mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]

        LocalChain
          .make[F](initialHead, initialHead, chainSelection, _ => Applicative[F].unit, blockHeights, emptyFetcher)
          .use(_.head.assertEquals(initialHead))
      }
    }
  }

  test("indicate when a new tine is worse than the local chain") {
    PropF.forAllF(genSizedStrictByteString[Lengths.`64`.type](), etaGen) { (rho, eta) =>
      withMock {
        val initialHead =
          SlotData(
            SlotId.of(1, blockId1),
            SlotId.of(0, blockId0),
            rho.data,
            eta.data,
            0
          )

        val newHead =
          SlotData(
            SlotId.of(2, blockId2),
            SlotId.of(1, blockId1),
            rho.data,
            eta.data,
            1
          )

        val blockHeights =
          mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]

        LocalChain
          .make[F](initialHead, initialHead, chainSelection, _ => Applicative[F].unit, blockHeights, emptyFetcher)
          .use(_.isWorseThan(NonEmptyChain.one(newHead)).assert)
      }
    }
  }

  test("adopt a new tine when instructed") {
    PropF.forAllF(genSizedStrictByteString[Lengths.`64`.type](), etaGen) { (rho, eta) =>
      withMock {
        val initialHead =
          SlotData(
            SlotId.of(1, blockId1),
            SlotId.of(0, blockId0),
            rho.data,
            eta.data,
            0
          )

        val newHead =
          SlotData(
            SlotId.of(2, blockId2),
            SlotId.of(1, blockId0),
            rho.data,
            eta.data,
            1
          )

        val blockHeights =
          mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]

        LocalChain
          .make[F](initialHead, initialHead, chainSelection, _ => Applicative[F].unit, blockHeights, emptyFetcher)
          .use(underTest =>
            underTest.head.assertEquals(initialHead) >>
            underTest.adopt(Validated.Valid(newHead)) >>
            underTest.head.assertEquals(newHead)
          )

      }
    }
  }

  test("Block IDs should be produced in a stream whenever they are adopted locally") {
    withMock {
      val initialHead =
        SlotData(
          SlotId.of(1, blockId1),
          SlotId.of(0, blockId0),
          genSizedStrictByteString[Lengths.`64`.type]().first.data,
          etaGen.first.data,
          0
        )

      val newHead =
        SlotData(
          SlotId.of(2, blockId2),
          SlotId.of(1, blockId0),
          genSizedStrictByteString[Lengths.`64`.type]().first.data,
          etaGen.first.data,
          1
        )

      val blockHeights =
        mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]

      LocalChain
        .make[F](initialHead, initialHead, chainSelection, _ => Applicative[F].unit, blockHeights, emptyFetcher)
        .use(underTest =>
          fs2.Stream
            .force(underTest.adoptions)
            .concurrently(
              fs2.Stream.exec(Async[F].delayBy(Async[F].defer(underTest.adopt(Validated.Valid(newHead))), 1.seconds))
            )
            .head
            .interruptAfter(3.seconds)
            .compile
            .lastOrError
            .assertEquals(newHead.slotId.blockId)
        )
    }
  }
}
