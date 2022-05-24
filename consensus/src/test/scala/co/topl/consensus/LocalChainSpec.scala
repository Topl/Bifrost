package co.topl.consensus

import cats.data.Validated
import cats.implicits._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models.ModelGenerators._
import co.topl.models.utility.Lengths
import co.topl.models._
import co.topl.typeclasses.OrderT
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class LocalChainSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {
  behavior of "LocalChain"

  type F[A] = IO[A]

  implicit private val ed25519Vrf: Ed25519VRF = Ed25519VRF.precomputed()

  it should "store the head of the local canonical tine" in {
    forAll(genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_)), etaGen) { (rho, eta) =>
      val initialHead =
        SlotData(SlotId(1, TypedBytes(1: Byte, Bytes(1))), SlotId(0, TypedBytes(0: Byte, Bytes(0))), rho, eta, 0)

      val chainSelection: OrderT[F, SlotData] = (a, b) => a.height.compareTo(b.height).pure[F]

      val underTest = LocalChain.Eval.make[F](initialHead, chainSelection).unsafeRunSync()

      underTest.head.unsafeRunSync() shouldBe initialHead
    }
  }

  it should "indicate when a new tine is worse than the local chain" in {
    forAll(genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_)), etaGen) { (rho, eta) =>
      val initialHead =
        SlotData(SlotId(1, TypedBytes(1: Byte, Bytes(1))), SlotId(0, TypedBytes(0: Byte, Bytes(0))), rho, eta, 0)

      val chainSelection: OrderT[F, SlotData] = (a, b) => a.height.compareTo(b.height).pure[F]

      val underTest = LocalChain.Eval.make[F](initialHead, chainSelection).unsafeRunSync()

      val newHead =
        SlotData(SlotId(2, TypedBytes(2: Byte, Bytes(2))), SlotId(1, TypedBytes(1: Byte, Bytes(0))), rho, eta, 1)

      underTest.isWorseThan(newHead).unsafeRunSync() shouldBe true
    }
  }

  it should "adopt a new tine when instructed" in {
    forAll(genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_)), etaGen) { (rho, eta) =>
      val initialHead =
        SlotData(SlotId(1, TypedBytes(1: Byte, Bytes(1))), SlotId(0, TypedBytes(0: Byte, Bytes(0))), rho, eta, 0)

      val chainSelection: OrderT[F, SlotData] = (a, b) => a.height.compareTo(b.height).pure[F]

      val underTest = LocalChain.Eval.make[F](initialHead, chainSelection).unsafeRunSync()

      val newHead =
        SlotData(SlotId(2, TypedBytes(2: Byte, Bytes(2))), SlotId(1, TypedBytes(1: Byte, Bytes(0))), rho, eta, 1)

      underTest.head.unsafeRunSync() shouldBe initialHead

      underTest.adopt(Validated.Valid(newHead)).unsafeRunSync()

      underTest.head.unsafeRunSync() shouldBe newHead

    }
  }
}
