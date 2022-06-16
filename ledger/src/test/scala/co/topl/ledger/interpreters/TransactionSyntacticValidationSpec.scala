package co.topl.ledger.interpreters

import cats.Applicative
import cats.data.{Chain, EitherT, NonEmptyChain}
import cats.effect._
import cats.implicits._
import co.topl.ledger.algebras.{InvalidSyntaxError, InvalidSyntaxErrors}
import co.topl.models.ModelGenerators._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized
import co.topl.models.{Box, Transaction}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.effect.PropF

class TransactionSyntacticValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("validate non-empty inputs") {
    PropF.forAllF(arbitraryTransaction.arbitrary.map(_.copy(inputs = Chain.empty))) { transaction: Transaction =>
      for {
        underTest <- TransactionSyntacticValidation.make[F]
        result    <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.contains(InvalidSyntaxErrors.EmptyInputs))
          .assert
      } yield ()
    }
  }

  test("validate positive timestamp") {
    PropF.forAllF(arbitraryTransaction.arbitrary.map(tx => tx.copy(chronology = tx.chronology.copy(creation = -1)))) {
      transaction: Transaction =>
        for {
          underTest <- TransactionSyntacticValidation.make[F]
          result    <- underTest.validate(transaction)
          _ <- EitherT
            .fromEither[F](result.toEither)
            .swap
            .exists(_.toList.contains(InvalidSyntaxErrors.InvalidTimestamp(-1)))
            .assert
        } yield ()
    }
  }

  test("validate positive output quantities") {
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
      (for {
        tx      <- arbitraryTransaction.arbitrary
        outputs <- Gen.listOf(outputGen)
      } yield tx.copy(outputs = Chain.fromSeq(outputs))).filter(transaction =>
        transaction.outputs.exists(o =>
          o.value match {
            case v: Box.Values.Poly  => v.quantity.data <= 0
            case v: Box.Values.Arbit => v.quantity.data <= 0
            case v: Box.Values.Asset => v.quantity.data <= 0
            case _                   => false
          }
        )
      )

    PropF.forAllF(negativeTransactionGen) { transaction: Transaction =>
      for {
        underTest <- TransactionSyntacticValidation.make[F]
        result    <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.exists {
            case _: InvalidSyntaxErrors.NonPositiveOutputValue => true
            case _                                             => false
          })
          .assert
      } yield ()
    }
  }

  test("validate sufficient input funds (Manual)") {
    def testTx(transaction: Transaction) =
      for {
        underTest <- TransactionSyntacticValidation.make[F]
        result    <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.exists {
            case InvalidSyntaxErrors.InsufficientInputFunds(_, _) => true
            case _                                                => false
          })
          .assert
      } yield ()
    // Manual tests
    PropF.forAllF { (baseTransaction: Transaction, input: Transaction.Input, output: Transaction.Output) =>
      // Poly Test
      testTx(
        baseTransaction.copy(
          inputs = Chain(
            input.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(1))))
          ),
          outputs = Chain(
            output.copy(value = Box.Values.Poly(Sized.maxUnsafe(BigInt(2))), minting = false)
          )
        )
      ) >>
      // Arbit Test
      testTx(
        baseTransaction.copy(
          inputs = Chain(
            input.copy(value = Box.Values.Arbit(Sized.maxUnsafe(BigInt(1))))
          ),
          outputs = Chain(
            output.copy(value = Box.Values.Arbit(Sized.maxUnsafe(BigInt(2))), minting = false)
          )
        )
      ) >>
      // Asset Test
      testTx {
        val assetCode = arbitraryAssetCode.arbitrary.first
        baseTransaction.copy(
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
      }
    }
  }

  test("validate sufficient input funds (Gen)") {
    // Arbitrary/generator tests
    PropF.forAllF(arbitraryTransaction.arbitrary) { transaction: Transaction =>
      val polyInSum =
        transaction.inputs.collect { case Transaction.Input(_, _, _, Box.Values.Poly(quantity)) =>
          quantity.data
        }.sumAll
      val polyOutSum =
        transaction.outputs.collect { case Transaction.Output(_, Box.Values.Poly(quantity), false) =>
          quantity.data
        }.sumAll
      val arbitInSum =
        transaction.inputs.collect { case Transaction.Input(_, _, _, Box.Values.Arbit(quantity)) =>
          quantity.data
        }.sumAll
      val arbitOutSum =
        transaction.outputs.collect { case Transaction.Output(_, Box.Values.Arbit(quantity), false) =>
          quantity.data
        }.sumAll

      val assetInSum =
        transaction.inputs.collect { case Transaction.Input(_, _, _, v: Box.Values.Asset) =>
          v.quantity.data
        }.sumAll
      val assetOutSum =
        transaction.outputs.collect { case Transaction.Output(_, v: Box.Values.Asset, false) =>
          v.quantity.data
        }.sumAll
      def existsInsufficientInputFunds(result: Either[NonEmptyChain[InvalidSyntaxError], Transaction]) =
        EitherT
          .fromEither[F](result)
          .swap
          .exists(_.toList.exists {
            case InvalidSyntaxErrors.InsufficientInputFunds(_, _) => true
            case _                                                => false
          })
          .assert
      for {
        underTest <- TransactionSyntacticValidation.make[F]
        result    <- underTest.validate(transaction).map(_.toEither)
        _         <- (polyOutSum > polyInSum).pure[F].ifM(existsInsufficientInputFunds(result), Applicative[F].unit)
        _         <- (arbitOutSum > arbitInSum).pure[F].ifM(existsInsufficientInputFunds(result), Applicative[F].unit)
        _         <- (assetOutSum > assetInSum).pure[F].ifM(existsInsufficientInputFunds(result), Applicative[F].unit)
      } yield ()
    }
  }
}
