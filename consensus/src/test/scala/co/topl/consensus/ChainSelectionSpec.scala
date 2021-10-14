package co.topl.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class ChainSelectionSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "ChainSelection"

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  it should "return 0 for equal tines" in {
    val slotData = createSlotData(10, SlotId(9, TypedBytes(1: Byte, Bytes(Array[Byte](9)))))

    val orderT = ChainSelection.orderT[F](mock[SlotDataCache[F]], kLookback = 1, sWindow = 1)

    orderT.compare(slotData, slotData).unsafeRunSync() shouldBe 0
  }

  it should "use longest-chain rule for tines shorter than the kLookback parameter" in {
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))))
    val ancestor = createSlotData(10, grandAncestor.slotId)
    val xSegment = LazyList
      .unfold(ancestor)(previous =>
        Some(createSlotData(previous.slotId.slot + 1, previous.slotId))
          .map(d => (d, d))
      )
      .take(10)
      .toList
    val ySegment = LazyList
      .unfold(ancestor)(previous =>
        Some(createSlotData(previous.slotId.slot + 1, previous.slotId))
          .map(d => (d, d))
      )
      .take(5)
      .toList

    val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

    val cache = mock[SlotDataCache[F]]

    (cache
      .get(_: TypedIdentifier))
      .expects(*)
      .anyNumberOfTimes()
      .onCall((id: TypedIdentifier) => allBlocks(id).pure[F])

    val orderT = ChainSelection.orderT[F](cache, kLookback = 100, sWindow = 1)

    orderT.compare(xSegment.last, ySegment.last).unsafeRunSync() should be > 0
  }

  it should "use (longest-chain -> lowest slot) rule for equal length tines shorter than the kLookback parameter" in {
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))))
    val ancestor = createSlotData(10, grandAncestor.slotId)
    val xSegment = LazyList
      .unfold(ancestor)(previous =>
        Some(createSlotData(previous.slotId.slot + 1, previous.slotId))
          .map(d => (d, d))
      )
      .take(10)
      .toList
    val ySegment = {
      val base = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 1, previous.slotId))
            .map(d => (d, d))
        )
        .take(9)
        .toList

      base :+ createSlotData(base.last.slotId.slot + 2, base.last.slotId)
    }

    xSegment.length shouldBe ySegment.length

    val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

    val cache = mock[SlotDataCache[F]]

    (cache
      .get(_: TypedIdentifier))
      .expects(*)
      .anyNumberOfTimes()
      .onCall((id: TypedIdentifier) => allBlocks(id).pure[F])

    val orderT = ChainSelection.orderT[F](cache, kLookback = 100, sWindow = 1)

    orderT.compare(xSegment.last, ySegment.last).unsafeRunSync() should be > 0
  }

  it should "use (longest-chain -> lowest rho) rule for equal length tines with equal best slot shorter than the kLookback parameter" in {
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))))
    val ancestor = createSlotData(10, grandAncestor.slotId)
    val xSegment = {
      val base = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 1, previous.slotId))
            .map(d => (d, d))
        )
        .take(9)
        .toList

      base :+ createSlotData(
        base.last.slotId.slot + 1,
        base.last.slotId,
        rho = Sized.strictUnsafe[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(1)))
      )
    }
    val ySegment = {
      val base = LazyList
        .unfold(ancestor)(previous =>
          Some(createSlotData(previous.slotId.slot + 1, previous.slotId))
            .map(d => (d, d))
        )
        .take(9)
        .toList

      base :+ createSlotData(
        base.last.slotId.slot + 1,
        base.last.slotId,
        rho = Sized.strictUnsafe[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(5)))
      )
    }

    xSegment.length shouldBe ySegment.length
    xSegment.last.slotId.slot shouldBe ySegment.last.slotId.slot
    BigInt(xSegment.last.rho.data.toArray) should be < BigInt(ySegment.last.rho.data.toArray)

    val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

    val cache = mock[SlotDataCache[F]]

    (cache
      .get(_: TypedIdentifier))
      .expects(*)
      .anyNumberOfTimes()
      .onCall((id: TypedIdentifier) => allBlocks(id).pure[F])

    val orderT = ChainSelection.orderT[F](cache, kLookback = 100, sWindow = 1)

    orderT.compare(xSegment.last, ySegment.last).unsafeRunSync() should be > 0
  }

  it should "use chain-density rule for tines longer than the kLookback parameter" in {
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))))
    val ancestor = createSlotData(10, grandAncestor.slotId)
    val xSegment = LazyList
      .unfold(ancestor)(previous =>
        Some(createSlotData(previous.slotId.slot + 1, previous.slotId))
          .map(d => (d, d))
      )
      .take(50)
      .toList

    val ySegment = LazyList
      .unfold(ancestor)(previous =>
        Some(createSlotData(previous.slotId.slot + 2, previous.slotId))
          .map(d => (d, d))
      )
      .take(50)
      .toList

    val allBlocks = (List(grandAncestor, ancestor) ++ xSegment ++ ySegment).map(d => d.slotId.blockId -> d).toMap

    val cache = mock[SlotDataCache[F]]

    (cache
      .get(_: TypedIdentifier))
      .expects(*)
      .anyNumberOfTimes()
      .onCall((id: TypedIdentifier) => allBlocks(id).pure[F])

    val orderT = ChainSelection.orderT[F](cache, kLookback = 10, sWindow = 20)

    orderT.compare(xSegment.last, ySegment.last).unsafeRunSync() should be > 0
  }

  private def createSlotData(
    slot:         Slot,
    parentSlotId: SlotId,
    rho:          Rho = Sized.strictUnsafe[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(0)))
  ): SlotData =
    SlotData(
      SlotId(slot, TypedBytes(1: Byte, genSizedStrictBytes[Lengths.`32`.type]().first.data)),
      parentSlotId = parentSlotId,
      rho = rho,
      eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))
    )

}
