package co.topl.ledger.interpreters

import cats.effect.IO
import cats.implicits._
import co.topl.brambl.generators.ModelGenerators._
import co.topl.brambl.models.box.{Attestation, FungibilityType, QuantityDescriptorType, Value}
import co.topl.brambl.models.transaction._
import co.topl.ledger.models.AssetId
import co.topl.models.ModelGenerators.GenHelper
import co.topl.brambl.syntax._
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import quivr.models.Int128

class TransactionRewardCalculatorSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  test("TransactionRewardCalculator returns empty list for empty Tx") {
    val tx = IoTransaction.defaultInstance
    val testResource =
      for {
        underTest <- TransactionRewardCalculator.make[F]
        rewards   <- underTest.rewardsOf(tx).pure[F].toResource
        _         <- IO(rewards.lvl).assertEquals(BigInt(0)).toResource
        _         <- IO(rewards.topl).assertEquals(BigInt(0)).toResource
        _         <- IO(rewards.assets).assertEquals(Map.empty[AssetId, BigInt]).toResource
      } yield ()

    testResource.use_
  }

  test("TransactionRewardCalculator returns unclaimed lvls, topls, and assets") {
    val lvlInputQuantities = List(500, 800)
    val lvlOutputQuantities = List(100)
    val toplInputQuantities = List(60, 30)
    val toplOutputQuantities = List(20, 10)
    val groupId1 = arbitraryGroupId.arbitrary.first
    val groupId2 = arbitraryGroupId.arbitrary.first
    val seriesId1 = arbitrarySeriesId.arbitrary.first
    val seriesId2 = arbitrarySeriesId.arbitrary.first
    val assetInputValues =
      List(
        Value(Value.Value.Asset(Value.Asset(groupId = groupId1.some, seriesId = seriesId1.some, quantity = 500))),
        Value(Value.Value.Asset(Value.Asset(groupId = groupId2.some, seriesId = seriesId2.some, quantity = 300)))
      )
    val assetOutputValues =
      List(
        Value(Value.Value.Asset(Value.Asset(groupId = groupId1.some, seriesId = seriesId1.some, quantity = 300))),
        Value(Value.Value.Asset(Value.Asset(groupId = groupId1.some, seriesId = seriesId1.some, quantity = 100))),
        Value(Value.Value.Asset(Value.Asset(groupId = groupId2.some, seriesId = seriesId2.some, quantity = 300)))
      )
    val inputs =
      (lvlInputQuantities.map(quantity => Value(Value.Value.Lvl(Value.LVL(quantity)))) ++
        toplInputQuantities.map(quantity => Value(Value.Value.Topl(Value.TOPL(quantity)))) ++
        assetInputValues)
        .map(value =>
          SpentTransactionOutput(arbitraryTransactionOutputAddress.arbitrary.first, Attestation.defaultInstance, value)
        )

    val outputs =
      (lvlOutputQuantities.map(quantity => Value(Value.Value.Lvl(Value.LVL(quantity)))) ++
        toplOutputQuantities.map(quantity => Value(Value.Value.Topl(Value.TOPL(quantity)))) ++
        assetOutputValues)
        .map(value => UnspentTransactionOutput(arbitraryLockAddress.arbitrary.first, value))
    val tx = IoTransaction.defaultInstance.withInputs(inputs).withOutputs(outputs)
    val testResource =
      for {
        underTest <- TransactionRewardCalculator.make[F]
        rewards   <- underTest.rewardsOf(tx).pure[F].toResource
        _         <- IO(rewards.lvl).assertEquals(BigInt(1200)).toResource
        _         <- IO(rewards.topl).assertEquals(BigInt(60)).toResource
        _ <- IO(rewards.assets)
          .assertEquals(
            Map(
              AssetId(
                groupId1.some,
                seriesId1.some,
                None,
                None,
                FungibilityType.GROUP_AND_SERIES,
                QuantityDescriptorType.LIQUID
              ) -> BigInt(100)
            )
          )
          .toResource
      } yield ()

    testResource.use_
  }

  test("TransactionRewardCalculator returns 0 if LVL outputs exceed LVL inputs") {
    val inputs =
      List(BigInt(500), BigInt(800))
        .map(quantity => Value(Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))))
        .map(value =>
          SpentTransactionOutput(arbitraryTransactionOutputAddress.arbitrary.first, Attestation.defaultInstance, value)
        )

    val outputs =
      List(BigInt(1500))
        .map(quantity => Value(Value.Value.Lvl(Value.LVL(Int128(ByteString.copyFrom(quantity.toByteArray))))))
        .map(value => UnspentTransactionOutput(arbitraryLockAddress.arbitrary.first, value))
    val tx = IoTransaction.defaultInstance.withInputs(inputs).withOutputs(outputs)
    val testResource =
      for {
        underTest <- TransactionRewardCalculator.make[F]
        rewards   <- underTest.rewardsOf(tx).pure[F].toResource
        _         <- IO(rewards.lvl).assertEquals(BigInt(0)).toResource
        _         <- IO(rewards.topl).assertEquals(BigInt(0)).toResource
        _         <- IO(rewards.assets).assertEquals(Map.empty[AssetId, BigInt]).toResource
      } yield ()

    testResource.use_
  }
}
