package co.topl.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.crypto.signing.Ed25519VRF
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

  // TODO: Use generators to account for edge cases

  it should "return 0 for equal tines" in {
    val slotData = createSlotData(10, SlotId(9, TypedBytes(1: Byte, Bytes(Array[Byte](9)))), height = 4)

    val orderT = ChainSelection.orderT[F](mock[SlotDataCache[F]], kLookback = 1, sWindow = 1)

    orderT.compare(slotData, slotData).unsafeRunSync() shouldBe 0
  }

  it should "use longest-chain rule for tines shorter than the kLookback parameter" in {
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))), height = 4)
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

    val cache = mock[SlotDataCache[F]]

    (cache
      .get(_: TypedIdentifier))
      .expects(*)
      .anyNumberOfTimes()
      .onCall((id: TypedIdentifier) => allBlocks(id).pure[F])

    val orderT = ChainSelection.orderT[F](cache, kLookback = 100, sWindow = 1)

    orderT.compare(xSegment.last, ySegment.last).unsafeRunSync() should be > 0
  }

  it should "use lowest-slot rule for equal length tines shorter than the kLookback parameter" in {
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))), height = 4)
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

  it should "use lowest-rho-test-hash rule for equal length tines with equal best slot shorter than the kLookback parameter" in {
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))), height = 4)
    val ancestor = createSlotData(10, grandAncestor.slotId, grandAncestor.height + 1)

    val List(rhoX, rhoY) =
      List
        .tabulate(2)(i => Rho(Sized.strictUnsafe[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(i.toByte)))))
        .sortBy(r => BigInt(Ed25519VRF.rhoToRhoTestHash(r).sizedBytes.data.toArray))

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

    xSegment.length shouldBe ySegment.length
    xSegment.last.slotId.slot shouldBe ySegment.last.slotId.slot

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
    val grandAncestor = createSlotData(9, SlotId(8, TypedBytes(1: Byte, Bytes(Array[Byte](9)))), height = 4)
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
    height:       Long,
    rho:          Rho = Rho(Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0))))
  ): SlotData =
    SlotData(
      SlotId(slot, TypedBytes(1: Byte, genSizedStrictBytes[Lengths.`32`.type]().first.data)),
      parentSlotId = parentSlotId,
      rho = rho,
      eta = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))),
      height = height
    )

}
