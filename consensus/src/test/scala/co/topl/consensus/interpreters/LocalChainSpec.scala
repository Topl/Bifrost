package co.topl.consensus.interpreters

import cats.Applicative
import cats.effect.IO
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.consensus.algebras.ChainSelectionAlgebra
import co.topl.consensus.models.{BlockId, SlotData, SlotId}
import co.topl.eventtree.EventSourcedState
import co.topl.interpreters.BlockHeightTree
import co.topl.models.ModelGenerators._
import co.topl.models.generators.common.ModelGenerators.genSizedStrictByteString
import co.topl.models.generators.consensus.ModelGenerators.etaGen
import co.topl.models.utility.Lengths
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import scala.concurrent.duration._

class LocalChainSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  type F[A] = IO[A]

  private val blockId0 = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(0)))
  private val blockId1 = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(1)))
  private val blockId2 = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(2)))

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

        LocalChain
          .make[F](initialHead, initialHead, chainSelection, _ => Applicative[F].unit, blockHeights)
          .use(_.head.assertEquals(initialHead))
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

        LocalChain
          .make[F](initialHead, initialHead, chainSelection, _ => Applicative[F].unit, blockHeights)
          .use(underTest =>
            underTest.head.assertEquals(initialHead) >>
            underTest.adopt(newHead) >>
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

      LocalChain
        .make[F](initialHead, initialHead, chainSelection, _ => Applicative[F].unit, blockHeights)
        .use(underTest =>
          fs2.Stream
            .force(underTest.adoptions)
            .concurrently(
              fs2.Stream.exec(Async[F].delayBy(Async[F].defer(underTest.adopt(newHead)), 1.seconds))
            )
            .head
            .interruptAfter(3.seconds)
            .compile
            .lastOrError
            .assertEquals(newHead.slotId.blockId)
        )
    }
  }

  test("Block IDs can be requested by height") {
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

      val _blockHeights = new EventSourcedState[F, BlockHeightTree.State[F], BlockId] {

        def stateAt(eventId: BlockId): F[BlockHeightTree.State[F]] =
          ???

        def useStateAt[U](eventId: BlockId)(f: BlockHeightTree.State[F] => F[U]): F[U] =
          eventId.pure[F].assertEquals(newHead.slotId.blockId) >> TestStore
            .make[F, Long, BlockId]
            .flatTap(_.put(0, initialHead.slotId.blockId))
            .flatMap(store => f(store.get))
      }

      LocalChain
        .make[F](initialHead, newHead, chainSelection, _ => Applicative[F].unit, _blockHeights)
        .use(underTest => underTest.blockIdAtHeight(0).assertEquals(initialHead.slotId.blockId.some))
    }
  }

  def chainSelection: ChainSelectionAlgebra[F] = mock[ChainSelectionAlgebra[F]]

  def blockHeights: EventSourcedState[F, BlockHeightTree.State[F], BlockId] =
    mock[EventSourcedState[F, BlockHeightTree.State[F], BlockId]]
}
