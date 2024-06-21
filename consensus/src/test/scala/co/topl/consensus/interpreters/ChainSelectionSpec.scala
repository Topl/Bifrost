package co.topl.consensus.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.utils.CatsUnsafeResource
import co.topl.consensus.models._
import co.topl.consensus.rhoToRhoTestHash
import co.topl.crypto.hash.Blake2b512
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.generators.common.ModelGenerators._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import co.topl.algebras.Stats.Implicits._

class ChainSelectionSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  implicit private val blake2b512: Blake2b512 = new Blake2b512

  private val blake2b512Resource =
    ResourceSuiteLocalFixture("blake2b512", CatsUnsafeResource.make[F, Blake2b512](new Blake2b512, 1).toResource)

  private val emptyFetcher = (_: BlockId) => Option.empty[SlotData].pure[F]
  override def munitFixtures = List(blake2b512Resource)

  // TODO: Use generators to account for edge cases

  test("return 0 for equal tines") {
    val slotData = createSlotData(10, SlotId(9, BlockId.of(ByteString.copyFrom(Array.fill[Byte](32)(9)))), height = 4)

    val orderT =
      ChainSelection
        .make[F](mockFunction[BlockId, F[SlotData]], blake2b512Resource(), kLookback = 1, sWindow = 1)

    orderT.compare(slotData, slotData, emptyFetcher).assertEquals(0)
  }

  test("use longest-chain rule for tines shorter than the kLookback parameter") {
    withMock {
      val grandAncestor =
        createSlotData(9, SlotId(8, BlockId.of(ByteString.copyFrom(Array.fill[Byte](32)(9)))), height = 4)
      val ancestor = createSlotData(10, grandAncestor.slotId, grandAncestor.height + 1)
      val xSegment = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
            .map(d => (d, d))
        )
        .take(10)
        .toList
      val ySegment = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
            .map(d => (d, d))
        )
        .take(5)
        .toList

      val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

      val fetchSlotData = mockFunction[BlockId, F[SlotData]]

      fetchSlotData
        .expects(*)
        .anyNumberOfTimes()
        .onCall((id: BlockId) => allBlocks(id).pure[F])

      val orderT = ChainSelection.make[F](fetchSlotData, blake2b512Resource(), kLookback = 100, sWindow = 1)

      orderT.compare(xSegment.last, ySegment.last, emptyFetcher).map(_ > 0).assert
    }

    test("use lowest-slot rule for equal length tines shorter than the kLookback parameter") {
      val grandAncestor =
        createSlotData(9, SlotId(8, BlockId.of(ByteString.copyFrom(Array.fill[Byte](32)(9)))), height = 4)
      val ancestor = createSlotData(10, grandAncestor.slotId, grandAncestor.height + 1)
      val xSegment = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
            .map(d => (d, d))
        )
        .take(10)
        .toList
      val ySegment = {
        val base = LazyList
          .unfold(ancestor)(previous =>
            Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
              .map(d => (d, d))
          )
          .take(9)
          .toList

        base :+ createSlotData(base.last.slotId.slot + 2, base.last.slotId, base.last.height + 1)
      }

      assert(xSegment.length == ySegment.length)

      val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

      val fetchSlotData = mockFunction[BlockId, F[SlotData]]

      fetchSlotData
        .expects(*)
        .anyNumberOfTimes()
        .onCall((id: BlockId) => allBlocks(id).pure[F])

      val orderT = ChainSelection.make[F](fetchSlotData, blake2b512Resource(), kLookback = 100, sWindow = 1)

      orderT.compare(xSegment.last, ySegment.last, emptyFetcher).map(_ > 0).assert
    }
  }

  test(
    "use lowest-rho-test-hash rule for equal length tines with equal best slot shorter than the kLookback parameter"
  ) {
    withMock {
      val grandAncestor =
        createSlotData(9, SlotId(8, BlockId.of(ByteString.copyFrom(Array.fill[Byte](32)(9)))), height = 4)
      val ancestor = createSlotData(10, grandAncestor.slotId, grandAncestor.height + 1)

      val List(rhoX, rhoY) =
        List
          .tabulate(2)(i =>
            Rho(Sized.strictUnsafe[Bytes, Lengths.`64`.type](ByteString.copyFrom(Array.fill[Byte](64)(i.toByte))))
          )
          .sortBy(r => BigInt(rhoToRhoTestHash(r.sizedBytes.data).toByteArray))

      val xSegment = {
        val base = LazyList
          .unfold(ancestor)(previous =>
            Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
              .map(d => (d, d))
          )
          .take(9)
          .toList

        base :+ createSlotData(
          base.last.slotId.slot + 1,
          base.last.slotId,
          base.last.height + 1,
          rho = rhoX
        )
      }
      val ySegment = {
        val base = LazyList
          .unfold(ancestor)(previous =>
            Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
              .map(d => (d, d))
          )
          .take(9)
          .toList

        base :+ createSlotData(
          base.last.slotId.slot + 1,
          base.last.slotId,
          base.last.height + 1,
          rho = rhoY
        )
      }

      assert(xSegment.length == ySegment.length)
      assert(xSegment.last.slotId.slot == ySegment.last.slotId.slot)

      val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

      val fetchSlotData = mockFunction[BlockId, F[SlotData]]

      fetchSlotData
        .expects(*)
        .anyNumberOfTimes()
        .onCall((id: BlockId) => allBlocks(id).pure[F])

      val orderT = ChainSelection.make[F](fetchSlotData, blake2b512Resource(), kLookback = 100, sWindow = 1)

      orderT.compare(xSegment.last, ySegment.last, emptyFetcher).map(_ > 0).assert
    }
  }

  test("use chain-density rule for tines longer than the kLookback parameter") {
    withMock {
      val grandAncestor =
        createSlotData(9, SlotId(8, BlockId.of(ByteString.copyFrom(Array.fill[Byte](32)(9)))), height = 4)
      val ancestor = createSlotData(10, grandAncestor.slotId, grandAncestor.height + 1)
      val xSegment = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
            .map(d => (d, d))
        )
        .take(50)
        .toList

      val ySegment = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 2, previous.slotId, previous.height + 1))
            .map(d => (d, d))
        )
        .take(50)
        .toList

      val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

      val fetchSlotData = mockFunction[BlockId, F[SlotData]]

      fetchSlotData
        .expects(*)
        .anyNumberOfTimes()
        .onCall((id: BlockId) => allBlocks(id).pure[F])

      val orderT = ChainSelection.make[F](fetchSlotData, blake2b512Resource(), kLookback = 10, sWindow = 20)

      orderT.compare(xSegment.last, ySegment.last, emptyFetcher).map(_ > 0).assert
    }

    test("tiebreak chain-density rule by rhoTestHash for equal density tines") {
      val grandAncestor =
        createSlotData(9, SlotId(8, BlockId.of(ByteString.copyFrom(Array.fill[Byte](32)(9)))), height = 4)
      val ancestor = createSlotData(10, grandAncestor.slotId, grandAncestor.height + 1)

      val List(rhoX, rhoY) =
        List
          .tabulate(2)(i =>
            Rho(Sized.strictUnsafe[Bytes, Lengths.`64`.type](ByteString.copyFrom(Array.fill[Byte](64)(i.toByte))))
          )
          .sortBy(r => BigInt(rhoToRhoTestHash(r.sizedBytes.data).toByteArray))

      val xSegment = {
        val base = LazyList
          .unfold(ancestor)(previous =>
            Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
              .map(d => (d, d))
          )
          .take(50)
          .toList

        base :+ createSlotData(
          base.last.slotId.slot + 1,
          base.last.slotId,
          base.last.height + 1,
          rho = rhoX
        )
      }
      val ySegment = {
        val base = LazyList
          .unfold(ancestor)(previous =>
            Some(createSlotData(previous.slotId.slot + 1, previous.slotId, previous.height + 1))
              .map(d => (d, d))
          )
          .take(50)
          .toList

        base :+ createSlotData(
          base.last.slotId.slot + 1,
          base.last.slotId,
          base.last.height + 1,
          rho = rhoY
        )
      }

      val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

      val fetchSlotData = mockFunction[BlockId, F[SlotData]]

      fetchSlotData
        .expects(*)
        .anyNumberOfTimes()
        .onCall((id: BlockId) => allBlocks(id).pure[F])

      val orderT = ChainSelection.make[F](fetchSlotData, blake2b512Resource(), kLookback = 0, sWindow = 150)

      orderT.compare(xSegment.last, ySegment.last, emptyFetcher).map(_ > 0).assert
    }
  }

  private def createSlotData(
    slot:         Slot,
    parentSlotId: SlotId,
    height:       Long,
    rho:          Rho = Rho(Sized.strictUnsafe(ByteString.copyFrom(Array.fill[Byte](64)(0))))
  ): SlotData =
    SlotData(
      SlotId(slot, BlockId.of(genSizedStrictByteString[Lengths.`32`.type]().first.data)),
      parentSlotId = parentSlotId,
      rho = rho.sizedBytes.data,
      eta = ByteString.copyFrom(Array.fill[Byte](32)(0)),
      height = height
    )

}
