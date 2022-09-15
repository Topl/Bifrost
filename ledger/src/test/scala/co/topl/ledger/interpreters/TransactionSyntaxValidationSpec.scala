package co.topl.ledger.interpreters

import cats.Applicative
import cats.data.{Chain, EitherT, NonEmptyChain}
import cats.effect._
import cats.implicits._
import co.topl.ledger.models._
import co.topl.models.ModelGenerators._
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized
import co.topl.models.{Box, Proof, Proofs, Proposition, Propositions, SecretKeys, Transaction}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import co.topl.typeclasses.implicits._

class TransactionSyntaxValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("validate non-empty inputs") {
    PropF.forAllF(arbitraryTransaction.arbitrary.map(_.copy(inputs = Chain.empty))) { transaction: Transaction =>
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        result    <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.contains(TransactionSyntaxErrors.EmptyInputs))
          .assert
      } yield ()
    }
  }

  test("validate distinct inputs") {
    PropF.forAllF { (_transaction: Transaction, input: Transaction.Input) =>
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        transaction = _transaction.copy(inputs = Chain(input, input))
        result <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.contains(TransactionSyntaxErrors.DuplicateInput(input.boxId)))
          .assert
      } yield ()
    }
  }

  test("validate maximum outputs count") {
    PropF.forAllF { (_transaction: Transaction, output: Transaction.Output) =>
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        transaction = _transaction.copy(outputs = Chain.fromSeq(Vector.fill(Short.MaxValue)(output)))
        result <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.contains(TransactionSyntaxErrors.ExcessiveOutputsCount))
          .assert
      } yield ()
    }
  }

  test("validate positive timestamp") {
    PropF.forAllF(arbitraryTransaction.arbitrary.map(tx => tx.copy(schedule = tx.schedule.copy(creation = -1)))) {
      transaction: Transaction =>
        for {
          underTest <- TransactionSyntaxValidation.make[F]
          result    <- underTest.validate(transaction)
          _ <- EitherT
            .fromEither[F](result.toEither)
            .swap
            .exists(_.toList.contains(TransactionSyntaxErrors.InvalidTimestamp(-1)))
            .assert
        } yield ()
    }
  }

  test("validate schedule") {
    PropF.forAllF(arbitraryTransaction.arbitrary) { transaction: Transaction =>
      val invalidTransaction1 = transaction.copy(schedule = transaction.schedule.copy(minimumSlot = 5, maximumSlot = 4))
      val invalidTransaction2 =
        transaction.copy(schedule = transaction.schedule.copy(minimumSlot = -5, maximumSlot = -1))
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        _ <- List(invalidTransaction1, invalidTransaction2).traverse(transaction =>
          EitherT(underTest.validate(transaction).map(_.toEither)).swap
            .exists(_.toList.contains(TransactionSyntaxErrors.InvalidSchedule(transaction.schedule)))
            .assert
        )
      } yield ()
    }
  }

  test("validate positive output quantities") {
    val boxValueGen =
      arbitraryBoxValue.arbitrary.flatMap {
        case Box.Values.Poly(_)    => arbitraryInt128.arbitrary.map(Box.Values.Poly)
        case Box.Values.Arbit(_)   => arbitraryInt128.arbitrary.map(Box.Values.Arbit)
        case a: Box.Values.AssetV1 => arbitraryInt128.arbitrary.map(q => a.copy(quantity = q))
        case v                     => Gen.const(v)
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
            case v: Box.Values.Poly    => v.quantity.data <= 0
            case v: Box.Values.Arbit   => v.quantity.data <= 0
            case v: Box.Values.AssetV1 => v.quantity.data <= 0
            case _                     => false
          }
        )
      )

    PropF.forAllF(negativeTransactionGen) { transaction: Transaction =>
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        result    <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.exists {
            case _: TransactionSyntaxErrors.NonPositiveOutputValue => true
            case _                                                 => false
          })
          .assert
      } yield ()
    }
  }

  test("validate sufficient input funds (Manual)") {
    def testTx(transaction: Transaction) =
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        result    <- underTest.validate(transaction)
        _ <- EitherT
          .fromEither[F](result.toEither)
          .swap
          .exists(_.toList.exists {
            case TransactionSyntaxErrors.InsufficientInputFunds(_, _) => true
            case _                                                    => false
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
        transaction.inputs.collect { case Transaction.Input(_, _, _, v: Box.Values.AssetV1) =>
          v.quantity.data
        }.sumAll
      val assetOutSum =
        transaction.outputs.collect { case Transaction.Output(_, v: Box.Values.AssetV1, false) =>
          v.quantity.data
        }.sumAll
      def existsInsufficientInputFunds(result: Either[NonEmptyChain[TransactionSyntaxError], Transaction]) =
        EitherT
          .fromEither[F](result)
          .swap
          .exists(_.toList.exists {
            case TransactionSyntaxErrors.InsufficientInputFunds(_, _) => true
            case _                                                    => false
          })
          .assert
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        result    <- underTest.validate(transaction).map(_.toEither)
        _         <- (polyOutSum > polyInSum).pure[F].ifM(existsInsufficientInputFunds(result), Applicative[F].unit)
        _         <- (arbitOutSum > arbitInSum).pure[F].ifM(existsInsufficientInputFunds(result), Applicative[F].unit)
        _         <- (assetOutSum > assetInSum).pure[F].ifM(existsInsufficientInputFunds(result), Applicative[F].unit)
      } yield ()
    }
  }

  test("validate proof types") {
    def createTestTransaction(proposition: Proposition, proof: Proof): Transaction =
      Transaction(
        Chain(arbitraryTransactionInput.arbitrary.first.copy(proposition = proposition, proof = proof)),
        Chain.empty,
        Transaction.Schedule(0L, 0L, Long.MaxValue),
        None
      )
    PropF.forAllF { (transaction: Transaction, curveSk: SecretKeys.Curve25519, edSK: SecretKeys.Ed25519) =>
      val transaction1 =
        createTestTransaction(curveSk.vk.asProposition, arbitraryProofsKnowledgeEd25519.arbitrary.first)
      val transaction2 =
        createTestTransaction(edSK.vk.asProposition, arbitraryProofsKnowledgeCurve25519.arbitrary.first)
      val transaction3 = createTestTransaction(
        curveSk.vk.asProposition and edSK.vk.asProposition,
        Proofs.Compositional.And(arbitraryProofsKnowledgeCurve25519.arbitrary.first, Proofs.Undefined)
      )
      val transaction4 = createTestTransaction(
        curveSk.vk.asProposition or edSK.vk.asProposition,
        Proofs.Compositional.Or(arbitraryProofsKnowledgeEd25519.arbitrary.first, Proofs.Undefined)
      )
      val transaction5 = createTestTransaction(
        List(curveSk.vk.asProposition, edSK.vk.asProposition).threshold(2),
        Proofs.Compositional
          .Or(arbitraryProofsKnowledgeEd25519.arbitrary.first, arbitraryProofsKnowledgeCurve25519.arbitrary.first)
      )
      val transaction6 = createTestTransaction(
        Propositions.Compositional.Not(curveSk.vk.asProposition),
        Proofs.Compositional.Not(arbitraryProofsKnowledgeEd25519.arbitrary.first)
      )
      for {
        underTest <- TransactionSyntaxValidation.make[F]
        testTransaction = (transaction: Transaction) =>
          EitherT(underTest.validate(transaction).map(_.toEither)).swap
            .exists(_.toList.exists(_.isInstanceOf[TransactionSyntaxErrors.InvalidProofType]))
            .assert
        _ <- testTransaction(transaction1)
        _ <- testTransaction(transaction2)
        _ <- testTransaction(transaction3)
        _ <- testTransaction(transaction4)
        _ <- testTransaction(transaction5)
        _ <- testTransaction(transaction6)
      } yield ()
    }
  }
}
