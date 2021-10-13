package co.topl.consensus

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
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
    val slotData = SlotData(
      SlotId(10, TypedBytes(1: Byte, Bytes(Array[Byte](10)))),
      SlotId(9, TypedBytes(1: Byte, Bytes(Array[Byte](9)))),
      Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(0))),
      Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))
    )
    val orderT = ChainSelection.orderT[F](mock[SlotDataCache[F]], kLookback = 1, sWindow = 1)

    orderT.compare(slotData, slotData).unsafeRunSync() shouldBe 0
  }
  it should "use longest-chain rule for tines shorter than the kLookback parameter" in {}
  it should "use chain-density rule for tines longer than the kLookback parameter" in {}

}
