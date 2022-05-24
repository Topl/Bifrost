package co.topl.ledger.interpreters

import cats.data.Chain
import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.ledger.algebras.InvalidSyntaxErrors
import co.topl.models.ModelGenerators._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Box, Transaction}
import org.scalacheck.Gen
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
    forAll(arbitraryTransaction.arbitrary.map(tx => tx.copy(chronology = tx.chronology.copy(creation = -1)))) {
      transaction: Transaction =>
        val result = SyntacticValidation
          .make[F]
          .flatMap(_.validateSyntax(transaction))
          .unsafeRunSync()

        result.toEither.left.value.toChain.toList should contain(InvalidSyntaxErrors.InvalidTimestamp(-1))
    }
  }

  it should "validate positive output quantities" in {
    val boxValueGen =
      arbitraryBoxValue.arbitrary.flatMap {
        case Box.Values.Poly(_)  => arbitraryInt128.arbitrary.map(Box.Values.Poly)
        case Box.Values.Arbit(_) => arbitraryInt128.arbitrary.map(Box.Values.Arbit)
        case a: Box.Values.Asset => arbitraryInt128.arbitrary.map(q => a.copy(quantity = q))
        case v                   => Gen.const(v)
      }
    val outputGen =
      for {
        o <- arbitraryTransactionOutput.arbitrary
        v <- boxValueGen
      } yield o.copy(value = v)
    val negativeTransactionGen =
      for {
        tx      <- arbitraryTransaction.arbitrary
        outputs <- Gen.listOf(outputGen)
      } yield tx.copy(outputs = Chain.fromSeq(outputs))

    forAll(negativeTransactionGen) { transaction: Transaction =>
      whenever(
        transaction.outputs.exists(o =>
          o.value match {
            case v: Box.Values.Poly  => v.quantity.data <= 0
            case v: Box.Values.Arbit => v.quantity.data <= 0
            case v: Box.Values.Asset => v.quantity.data <= 0
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
    // Manual tests
    forAll { (baseTransaction: Transaction, input: Transaction.Input, output: Transaction.Output) =>
      // Poly Test
      {
        val tx = baseTransaction.copy(
          inputs = Chain(
            input.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(1))))
          ),
          outputs = Chain(
            output.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(2))), minting = false)
          )
        )
        val result = SyntacticValidation
          .make[F]
          .flatMap(_.validateSyntax(tx))
          .unsafeRunSync()
        result.toEither.left.value.exists {
          case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
          case _                                                => false
        } shouldBe true
      }
      // Arbit Test
      {
        val tx = baseTransaction.copy(
          inputs = Chain(
            input.copy(value = Box.Values.Arbit(Sized.maxUnsafe(BigInt(1))))
          ),
          outputs = Chain(
            output.copy(value = Box.Values.Arbit(Sized.maxUnsafe(BigInt(2))), minting = false)
          )
        )
        val result = SyntacticValidation
          .make[F]
          .flatMap(_.validateSyntax(tx))
          .unsafeRunSync()
        result.toEither.left.value.exists {
          case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
          case _                                                => false
        } shouldBe true
      }
      // Asset Test
      {
        val assetCode = arbitraryAssetCode.arbitrary.first
        val tx = baseTransaction.copy(
          inputs = Chain(
            input.copy(value =
              arbitraryAssetBox.arbitrary.first.copy(quantity = Sized.maxUnsafe(BigInt(1)), assetCode = assetCode)
            )
          ),
          outputs = Chain(
            output.copy(
              value =
                arbitraryAssetBox.arbitrary.first.copy(quantity = Sized.maxUnsafe(BigInt(2)), assetCode = assetCode),
              minting = false
            )
          )
        )
        val result = SyntacticValidation
          .make[F]
          .flatMap(_.validateSyntax(tx))
          .unsafeRunSync()
        result.toEither.left.value.exists {
          case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
          case _                                                => false
        } shouldBe true
      }
    }

    // Arbitrary/generator tests
    forAll(arbitraryTransaction.arbitrary, maxDiscardedFactor(100)) { transaction: Transaction =>
      val result = SyntacticValidation
        .make[F]
        .flatMap(_.validateSyntax(transaction))
        .unsafeRunSync()

      val polyInSum =
        transaction.inputs.collect { case Transaction.Input(_, _, _, _, Box.Values.Poly(quantity)) =>
          quantity.data
        }.sumAll
      val polyOutSum =
        transaction.outputs.collect { case Transaction.Output(_, Box.Values.Poly(quantity), false) =>
          quantity.data
        }.sumAll
      whenever(polyOutSum > polyInSum) {
        result.toEither.left.value.exists {
          case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
          case _                                                => false
        } shouldBe true
      }

      val arbitInSum =
        transaction.inputs.collect { case Transaction.Input(_, _, _, _, Box.Values.Arbit(quantity)) =>
          quantity.data
        }.sumAll
      val arbitOutSum =
        transaction.outputs.collect { case Transaction.Output(_, Box.Values.Arbit(quantity), false) =>
          quantity.data
        }.sumAll
      whenever(arbitOutSum > arbitInSum) {
        result.toEither.left.value.exists {
          case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
          case _                                                => false
        } shouldBe true
      }

      val assetInSum =
        transaction.inputs.collect { case Transaction.Input(_, _, _, _, v: Box.Values.Asset) =>
          v.quantity.data
        }.sumAll
      val assetOutSum =
        transaction.outputs.collect { case Transaction.Output(_, v: Box.Values.Asset, false) =>
          v.quantity.data
        }.sumAll
      whenever(assetOutSum > assetInSum) {
        result.toEither.left.value.exists {
          case _: InvalidSyntaxErrors.InsufficientInputFunds[_] => true
          case _                                                => false
        } shouldBe true
      }
    }
  }
}
