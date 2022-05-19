package co.topl.ledger.interpreters

import cats.data.Chain
import cats.effect._
import cats.effect.unsafe.implicits.global
import co.topl.ledger.algebras.InvalidSyntaxErrors
import co.topl.models.ModelGenerators._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized
import co.topl.models.{Box, Transaction}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SyntacticValidationSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with EitherValues {

  type F[A] = IO[A]

  behavior of "SyntacticValidation"

  it should "validate non-empty inputs" in {
    forAll(arbitraryTransaction.arbitrary.map(_.copy(inputs = Chain.empty))) { transaction: Transaction =>
      val result = SyntacticValidation
        .make[F]
        .flatMap(_.validateSyntax(transaction))
        .unsafeRunSync()

      result.toEither.left.value.toChain.toList should contain(InvalidSyntaxErrors.EmptyInputs)
    }
  }

  it should "validate positive timestamp" in {
    forAll(arbitraryTransaction.arbitrary.map(_.copy(timestamp = -1))) { transaction: Transaction =>
      val result = SyntacticValidation
        .make[F]
        .flatMap(_.validateSyntax(transaction))
        .unsafeRunSync()

      result.toEither.left.value.toChain.toList should contain(InvalidSyntaxErrors.InvalidTimestamp(-1))
    }
  }

  it should "validate positive output quantities" in {
    val negativeBoxValueGen =
      arbitraryBoxValue.arbitrary.map {
        case Box.Values.Poly(_)  => Box.Values.Poly(Sized.maxUnsafe(BigInt(-1)))
        case Box.Values.Arbit(_) => Box.Values.Arbit(Sized.maxUnsafe(BigInt(-1)))
        case a: Box.Values.Asset => a.copy(quantity = Sized.maxUnsafe(BigInt(-1)))
        case v                   => v
      }
    val negativeOutputGen =
      for {
        o <- arbitraryTransactionOutput.arbitrary
        v <- negativeBoxValueGen
      } yield o.copy(value = v)
    val negativeTransactionGen =
      for {
        tx      <- arbitraryTransaction.arbitrary
        outputs <- nonEmptyChainOf(negativeOutputGen)
      } yield tx.copy(outputs = outputs.toChain)

    forAll(negativeTransactionGen) { transaction: Transaction =>
      val result = SyntacticValidation
        .make[F]
        .flatMap(_.validateSyntax(transaction))
        .unsafeRunSync()

      result.toEither.left.value.exists {
        case _: InvalidSyntaxErrors.NonPositiveOutputValue => true
        case _                                             => false
      } shouldBe true
    }
  }
}
