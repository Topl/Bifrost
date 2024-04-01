package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.box.{FungibilityType, Lock, QuantityDescriptorType, Value}
import co.topl.brambl.models.transaction.{IoTransaction, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.models.{GroupId, LockAddress, SeriesId, TransactionId}
import co.topl.brambl.syntax._
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.ledger.algebras.TransactionRewardCalculatorAlgebra
import co.topl.ledger.models.{AssetId, BodySyntaxErrors, RewardQuantities}
import co.topl.models.ModelGenerators._
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

class BodySyntaxValidationSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("validation should fail if any transaction is syntactically invalid") {
    PropF.forAllF { transaction: IoTransaction =>
      withMock {
        val body = BlockBody(List(transaction.id))
        for {
          fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
          _ = fetchTransaction.expects(transaction.id).once().returning(transaction.pure[F])
          transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
          _ = (transactionSyntaxValidation.validate _)
            .expects(transaction)
            .once()
            .returning(
              (TransactionSyntaxError.EmptyInputs: TransactionSyntaxError).invalidNec[IoTransaction].toEither.pure[F]
            )
          rewardCalculator = mock[TransactionRewardCalculatorAlgebra]
          underTest <- BodySyntaxValidation.make[F](fetchTransaction, transactionSyntaxValidation, rewardCalculator)
          result    <- underTest.validate(body)
          _         <- IO(result.isInvalid).assert
        } yield ()
      }
    }
  }

  test("validation should fail if the reward is incorrect") {
    withMock {
      val transaction = arbitraryIoTransaction.arbitrary.first
      def test(lvlReward: Option[BigInt], toplReward: Option[BigInt], assetRewards: List[(AssetId, BigInt)])(
        rewardOutput:  RewardQuantities,
        expectSuccess: Boolean
      ) = {
        val lock: Lock = Lock().withPredicate(Lock.Predicate())

        val lockAddress: LockAddress =
          lock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
        val rewardTx = IoTransaction.defaultInstance
          .withInputs(
            List(
              SpentTransactionOutput(
                arbitraryTransactionOutputAddress.arbitrary.first,
                arbitraryAttestation.arbitrary.first,
                Value.defaultInstance.withLvl(Value.LVL(100L))
              )
            )
          )
          .withOutputs(
            List(
              lvlReward.map(q =>
                UnspentTransactionOutput(
                  lockAddress,
                  Value.defaultInstance.withLvl(Value.LVL(q))
                )
              ),
              toplReward.map(q =>
                UnspentTransactionOutput(
                  lockAddress,
                  Value.defaultInstance.withTopl(Value.TOPL(q))
                )
              )
            ).flatten ++ assetRewards.map { case (id, q) =>
              UnspentTransactionOutput(
                lockAddress,
                Value.defaultInstance.withAsset(
                  Value.Asset(
                    id.groupId,
                    id.seriesId,
                    q,
                    id.groupAlloy,
                    id.seriesAlloy,
                    id.fungibilityType,
                    id.quantityDescriptor
                  )
                )
              )
            }
          )
          .embedId
        val body = BlockBody(List(transaction.id), rewardTx.id.some)
        for {
          fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
          _ = fetchTransaction.expects(transaction.id).once().returning(transaction.pure[F])
          _ = fetchTransaction.expects(rewardTx.id).once().returning(rewardTx.pure[F])
          transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
          _ = (transactionSyntaxValidation.validate _)
            .expects(transaction)
            .once()
            .returning(transaction.validNec[TransactionSyntaxError].toEither.pure[F])
          rewardCalculator = mock[TransactionRewardCalculatorAlgebra]
          _ = (rewardCalculator
            .rewardsOf(_))
            .expects(transaction)
            .once()
            .returning(rewardOutput)
          underTest <- BodySyntaxValidation.make[F](fetchTransaction, transactionSyntaxValidation, rewardCalculator)
          result    <- underTest.validate(body)
          _         <- IO(result.isValid == expectSuccess).assert
          _ <- IO(
            expectSuccess || result.swap.toOption.get.toList == List(BodySyntaxErrors.InvalidReward(rewardTx))
          ).assert
        } yield ()
      }
      test(BigInt(100).some, none, Nil)(RewardQuantities(BigInt(2L), BigInt(0), Map.empty), expectSuccess = false) *>
      test(BigInt(100).some, none, Nil)(RewardQuantities(BigInt(200L), BigInt(0), Map.empty), expectSuccess = true) *>
      test(BigInt(100).some, BigInt(100).some, Nil)(
        RewardQuantities(BigInt(200L), BigInt(0), Map.empty),
        expectSuccess = false
      ) *>
      test(BigInt(100).some, BigInt(100).some, Nil)(
        RewardQuantities(BigInt(200L), BigInt(100L), Map.empty),
        expectSuccess = true
      ) *> {
        val assetId1 = AssetId(
          groupId = GroupId(ByteString.copyFrom(Array.fill[Byte](32)(1))).some,
          seriesId = SeriesId(ByteString.copyFrom(Array.fill[Byte](32)(2))).some,
          groupAlloy = none,
          seriesAlloy = none,
          fungibilityType = FungibilityType.GROUP_AND_SERIES,
          quantityDescriptor = QuantityDescriptorType.LIQUID
        )
        val assetId2 = AssetId(
          groupId = GroupId(ByteString.copyFrom(Array.fill[Byte](32)(3))).some,
          seriesId = SeriesId(ByteString.copyFrom(Array.fill[Byte](32)(4))).some,
          groupAlloy = none,
          seriesAlloy = none,
          fungibilityType = FungibilityType.GROUP_AND_SERIES,
          quantityDescriptor = QuantityDescriptorType.LIQUID
        )
        val assetId3 = AssetId(
          groupId = GroupId(ByteString.copyFrom(Array.fill[Byte](32)(4))).some,
          seriesId = SeriesId(ByteString.copyFrom(Array.fill[Byte](32)(5))).some,
          groupAlloy = none,
          seriesAlloy = none,
          fungibilityType = FungibilityType.GROUP_AND_SERIES,
          quantityDescriptor = QuantityDescriptorType.LIQUID
        )
        test(none, none, List(assetId1 -> BigInt(100)))(
          RewardQuantities(BigInt(0), BigInt(0), Map(assetId1 -> BigInt(100), assetId2 -> BigInt(100))),
          expectSuccess = true
        ) *>
        test(none, none, List(assetId1 -> BigInt(100), assetId2 -> BigInt(50)))(
          RewardQuantities(BigInt(0), BigInt(0), Map(assetId1 -> BigInt(100), assetId2 -> BigInt(100))),
          expectSuccess = true
        ) *>
        test(none, none, List(assetId1 -> BigInt(150), assetId2 -> BigInt(50)))(
          RewardQuantities(BigInt(0), BigInt(0), Map(assetId1 -> BigInt(100), assetId2 -> BigInt(100))),
          expectSuccess = false
        ) *>
        test(none, none, List(assetId3 -> BigInt(150)))(
          RewardQuantities(BigInt(0), BigInt(0), Map.empty),
          expectSuccess = false
        )
      }
    }
  }

  test("validation should fail if a reward is provided for an empty block") {
    withMock {
      val lock: Lock = Lock().withPredicate(Lock.Predicate())

      val lockAddress: LockAddress =
        lock.lockAddress(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_LEDGER_ID)
      val rewardTx = IoTransaction.defaultInstance
        .withInputs(
          List(
            SpentTransactionOutput(
              arbitraryTransactionOutputAddress.arbitrary.first,
              arbitraryAttestation.arbitrary.first,
              Value().withLvl(Value.LVL(100L))
            )
          )
        )
        .withOutputs(
          List(
            UnspentTransactionOutput(
              lockAddress,
              Value().withLvl(Value.LVL(100L))
            )
          )
        )
        .embedId
      val body = BlockBody(Nil, rewardTx.id.some)
      for {
        fetchTransaction <- mockFunction[TransactionId, F[IoTransaction]].pure[F]
        _ = fetchTransaction.expects(rewardTx.id).once().returning(rewardTx.pure[F])
        transactionSyntaxValidation = mock[TransactionSyntaxVerifier[F]]
        rewardCalculator = mock[TransactionRewardCalculatorAlgebra]
        underTest <- BodySyntaxValidation.make[F](fetchTransaction, transactionSyntaxValidation, rewardCalculator)
        result    <- underTest.validate(body)
        _         <- IO(result.isInvalid).assert
        _         <- IO(result.swap.toOption.get.toList == List(BodySyntaxErrors.InvalidReward(rewardTx))).assert
      } yield ()
    }
  }
}
