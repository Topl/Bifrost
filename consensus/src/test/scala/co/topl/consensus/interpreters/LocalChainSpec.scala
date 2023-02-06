package co.topl.consensus.interpreters

import cats.Applicative
import cats.data.Validated
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.consensus.algebras.ChainSelectionAlgebra
import co.topl.models.ModelGenerators._
import co.topl.models._
import co.topl.models.utility.Lengths
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

  it should "store the head of the local canonical tine" in {
    forAll(genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_)), etaGen) { (rho, eta) =>
      val initialHead =
        SlotData(SlotId(1, TypedBytes(1: Byte, Bytes(1))), SlotId(0, TypedBytes(0: Byte, Bytes(0))), rho, eta, 0)

      val chainSelection: ChainSelectionAlgebra[F, SlotData] = (a, b) => a.height.compareTo(b.height).pure[F]

      val underTest = LocalChain.make[F](initialHead, chainSelection, _ => Applicative[F].unit).unsafeRunSync()

      underTest.head.unsafeRunSync() shouldBe initialHead
    }
  }

  it should "indicate when a new tine is worse than the local chain" in {
    forAll(genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_)), etaGen) { (rho, eta) =>
      val initialHead =
        SlotData(SlotId(1, TypedBytes(1: Byte, Bytes(1))), SlotId(0, TypedBytes(0: Byte, Bytes(0))), rho, eta, 0)

      val chainSelection: ChainSelectionAlgebra[F, SlotData] = (a, b) => a.height.compareTo(b.height).pure[F]

      val underTest = LocalChain.make[F](initialHead, chainSelection, _ => Applicative[F].unit).unsafeRunSync()

      val newHead =
        SlotData(SlotId(2, TypedBytes(2: Byte, Bytes(2))), SlotId(1, TypedBytes(1: Byte, Bytes(0))), rho, eta, 1)

      underTest.isWorseThan(newHead).unsafeRunSync() shouldBe true
    }
  }

  it should "adopt a new tine when instructed" in {
    forAll(genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_)), etaGen) { (rho, eta) =>
      val initialHead =
        SlotData(SlotId(1, TypedBytes(1: Byte, Bytes(1))), SlotId(0, TypedBytes(0: Byte, Bytes(0))), rho, eta, 0)

      val chainSelection: ChainSelectionAlgebra[F, SlotData] = (a, b) => a.height.compareTo(b.height).pure[F]

      val underTest = LocalChain.make[F](initialHead, chainSelection, _ => Applicative[F].unit).unsafeRunSync()

      val newHead =
        SlotData(SlotId(2, TypedBytes(2: Byte, Bytes(2))), SlotId(1, TypedBytes(1: Byte, Bytes(0))), rho, eta, 1)

      underTest.head.unsafeRunSync() shouldBe initialHead

      underTest.adopt(Validated.Valid(newHead)).unsafeRunSync()

      underTest.head.unsafeRunSync() shouldBe newHead

    }
  }
}
