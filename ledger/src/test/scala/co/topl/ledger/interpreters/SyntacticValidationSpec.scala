package co.topl.ledger.interpreters

import cats.implicits._
import cats.data.Chain
import cats.effect._
import cats.effect.unsafe.implicits.global
import co.topl.ledger.algebras.InvalidSyntaxErrors
import co.topl.models.ModelGenerators._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized
import co.topl.models.{Box, Transaction}
import org.scalacheck.{Arbitrary, Gen}
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
      whenever(
        transaction.outputs.exists(o =>
          o.value match {
            case v: Box.Values.Poly  => true
            case v: Box.Values.Arbit => true
            case v: Box.Values.Asset => true
            case _                   => false
          }
        )
      ) {
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

  it should "validate sufficient input funds" in {
    def typedInGen[T <: Box.Value: Arbitrary] =
      for {
        o <- arbitraryTransactionInput.arbitrary
        v <- implicitly[Arbitrary[T]].arbitrary
      } yield o.copy(value = v)
    def typedOutGen[T <: Box.Value: Arbitrary] =
      for {
        o <- arbitraryTransactionOutput.arbitrary
        v <- implicitly[Arbitrary[T]].arbitrary
      } yield o.copy(value = v)
    def txGen[T <: Box.Value: Arbitrary] =
      for {
        tx      <- arbitraryTransaction.arbitrary
        inputs  <- Gen.listOf(typedInGen[T])
        outputs <- Gen.listOf(typedOutGen[T])
      } yield tx.copy(inputs = Chain.fromSeq(inputs), outputs = Chain.fromSeq(outputs))

    def runTest[T <: Box.Value: Arbitrary](quantity: T => BigInt) = {
      forAll(txGen[T]) { transaction: Transaction =>
        val result = SyntacticValidation
          .make[F]
          .flatMap(_.validateSyntax(transaction))
          .unsafeRunSync()
        val inSum = transaction.inputs.map(_.value.asInstanceOf[T]).map(quantity).sumAll
        val outSum = transaction.outputs.filterNot(_.minting).map(_.value.asInstanceOf[T]).map(quantity).sumAll
        whenever(outSum > inSum)(
          result.toEither.left.value.exists {
            case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
            case _                                                => false
          } shouldBe true
        )
      }
      forAll(txGen[T]) { transaction: Transaction =>
        val result = SyntacticValidation
          .make[F]
          .flatMap(_.validateSyntax(transaction))
          .unsafeRunSync()
        val inSum = transaction.inputs.map(_.value.asInstanceOf[T]).map(quantity).sumAll
        val outSum = transaction.outputs.filterNot(_.minting).map(_.value.asInstanceOf[T]).map(quantity).sumAll
        whenever(inSum >= outSum)(
          result.toEither match {
            case Right(_) =>
              assert(true)
            case Left(errors) =>
              assert(
                !errors.exists {
                  case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
                  case _                                                => false
                }
              )
          }
        )
      }
    }

    runTest[Box.Values.Poly](_.quantity.data)
    runTest[Box.Values.Arbit](_.quantity.data)
    runTest[Box.Values.Asset](_.quantity.data)
  }
}
