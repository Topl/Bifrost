package co.topl.genus

import co.topl.genus.filters._
import co.topl.utils.mongodb.models._
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.immutable.ListMap

trait ArbitraryInstances {

  implicit val stringSelectionArbitrary: Arbitrary[StringSelection] =
    Arbitrary(Gen.listOf(Gen.asciiStr).map(StringSelection.of))

  implicit val minNumberRangeFilterTypeArbitrary: Arbitrary[NumberRange.FilterType.Min] =
    Arbitrary(Gen.chooseNum(Long.MinValue, Long.MaxValue).map(NumberRange.FilterType.Min.apply))

  implicit val maxNumberRangeFilterTypeArbitrary: Arbitrary[NumberRange.FilterType.Max] =
    Arbitrary(Gen.chooseNum(Long.MinValue, Long.MaxValue).map(NumberRange.FilterType.Max.apply))

  implicit val numberRangeFilterTypeArbitrary: Arbitrary[NumberRange.FilterType] =
    Arbitrary(
      Gen.oneOf(
        minNumberRangeFilterTypeArbitrary.arbitrary,
        maxNumberRangeFilterTypeArbitrary.arbitrary,
        Gen.const(NumberRange.FilterType.Empty)
      )
    )

  implicit val numberRangeArbitrary: Arbitrary[NumberRange] =
    Arbitrary(numberRangeFilterTypeArbitrary.arbitrary.map(NumberRange.of))

  implicit val numberSelectionArbitrary: Arbitrary[NumberSelection] =
    Arbitrary(Gen.listOf(Gen.chooseNum(Long.MinValue, Long.MaxValue)).map(NumberSelection.of))

  implicit val booleanSelectionArbitrary: Arbitrary[BooleanSelection] =
    Arbitrary(Gen.oneOf(true, false).map(BooleanSelection.of))

  implicit val tokenValueFilterArbitrary: Arbitrary[TokenValueFilter] =
    Arbitrary(
      Gen
        .oneOf(
          stringSelectionArbitrary.arbitrary.map(TokenValueFilter.FilterType.AssetCodeSelection.apply),
          numberRangeArbitrary.arbitrary.map(TokenValueFilter.FilterType.QuantityRange.apply),
          stringSelectionArbitrary.arbitrary.map(TokenValueFilter.FilterType.TokenValueTypeSelection.apply)
        )
        .map(TokenValueFilter.of)
    )

  implicit val transactionFilterArbitrary: Arbitrary[TransactionFilter] =
    Arbitrary(
      Gen
        .oneOf(
          stringSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.InputAddressSelection.apply),
          numberRangeArbitrary.arbitrary
            .map(TransactionFilter.FilterType.TimestampRange.apply),
          stringSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.InputAddressSelection.apply),
          numberSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.InputNonceSelection.apply),
          stringSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.OutputTokenBoxTypeSelection.apply),
          tokenValueFilterArbitrary.arbitrary
            .map(TransactionFilter.FilterType.OutputTokenValueFilter.apply),
          stringSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.OutputAddressSelection.apply),
          booleanSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.MintingSelection.apply),
          stringSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.TxIdSelection.apply),
          stringSelectionArbitrary.arbitrary
            .map(TransactionFilter.FilterType.BoxesToRemoveSelection.apply),
          numberRangeArbitrary.arbitrary
            .map(TransactionFilter.FilterType.FeeRange.apply),
          Gen.const(TransactionFilter.FilterType.All.apply(TransactionFilter.AllFilter.of()))
        )
        .map(TransactionFilter.of)
    )

  implicit val blockFilterArbitrary: Arbitrary[BlockFilter] =
    Arbitrary(
      Gen
        .oneOf(
          stringSelectionArbitrary.arbitrary
            .map(BlockFilter.FilterType.IdSelection.apply),
          stringSelectionArbitrary.arbitrary
            .map(BlockFilter.FilterType.ParentIdSelection.apply),
          numberRangeArbitrary.arbitrary
            .map(BlockFilter.FilterType.TimestampRange.apply),
          tokenValueFilterArbitrary.arbitrary
            .map(BlockFilter.FilterType.GeneratorBoxTokenValueFilter.apply),
          stringSelectionArbitrary.arbitrary
            .map(BlockFilter.FilterType.PublicKeySelection.apply),
          numberRangeArbitrary.arbitrary
            .map(BlockFilter.FilterType.HeightRange.apply),
          numberSelectionArbitrary.arbitrary
            .map(BlockFilter.FilterType.HeightSelection.apply),
          numberRangeArbitrary.arbitrary
            .map(BlockFilter.FilterType.DifficultyRange.apply),
          numberSelectionArbitrary.arbitrary
            .map(BlockFilter.FilterType.VersionSelection.apply),
          numberRangeArbitrary.arbitrary
            .map(BlockFilter.FilterType.NumTransactionRange.apply)
        )
        .map(BlockFilter.of)
    )

  implicit val blockSummaryDataModelArbitrary: Arbitrary[BlockSummaryDataModel] =
    Arbitrary(
      Gen.zip(Gen.asciiStr, Gen.posNum[Long]).map(values => BlockSummaryDataModel(values._1, values._2))
    )

  implicit val simpleValueDataModelArbitrary: Arbitrary[SimpleValueDataModel] =
    Arbitrary(
      Gen.posNum[BigInt].map(value => SimpleValueDataModel(value.toString()))
    )

  implicit val assetValueDataModelArbitrary: Arbitrary[AssetValueDataModel] =
    Arbitrary(
      Gen
        .zip(
          Gen.asciiStr,
          Gen.posNum[BigInt].map(_.toString()),
          Gen.asciiStr,
          Gen.option(Gen.asciiStr)
        )
        .map(values =>
          AssetValueDataModel(
            values._1,
            values._2,
            values._3,
            values._4
          )
        )
    )

  implicit val tokenValueDataModelArbitrary: Arbitrary[TokenValueDataModel] =
    Arbitrary(
      Gen.oneOf(simpleValueDataModelArbitrary.arbitrary, assetValueDataModelArbitrary.arbitrary)
    )

  implicit val tokenBoxDataModelArbitrary: Arbitrary[TokenBoxDataModel] =
    Arbitrary(
      Gen
        .zip(
          Gen.asciiStr,
          Gen.asciiStr,
          Gen.posNum[BigInt].map(_.toString()),
          Gen.asciiStr,
          tokenValueDataModelArbitrary.arbitrary
        )
        .map(values =>
          TokenBoxDataModel(
            values._1,
            values._2,
            values._3,
            values._4,
            values._5
          )
        )
    )

  implicit val confirmedTransactionDataModelArbitrary: Arbitrary[ConfirmedTransactionDataModel] =
    Arbitrary(
      for {
        blockSummary    <- blockSummaryDataModelArbitrary.arbitrary
        txType          <- Gen.oneOf(Seq("PolyTransfer", "ArbitTransfer", "AssetTransfer"))
        timestamp       <- Gen.posNum[BigInt].map(_.toString())
        signatures      <- Gen.listOf(Gen.zip(Gen.asciiStr, Gen.asciiStr))
        newBoxes        <- Gen.listOf(tokenBoxDataModelArbitrary.arbitrary)
        data            <- Gen.option(Gen.asciiStr)
        from            <- Gen.listOf(Gen.zip(Gen.asciiStr, Gen.asciiStr))
        minting         <- Gen.oneOf(true, false)
        txId            <- Gen.asciiStr
        boxesToRemove   <- Gen.listOf(Gen.asciiStr)
        fee             <- Gen.posNum[BigInt].map(_.toString())
        to              <- Gen.listOf(Gen.zip(Gen.asciiStr, tokenValueDataModelArbitrary.arbitrary))
        propositionType <- Gen.asciiStr
      } yield new ConfirmedTransactionDataModel(
        blockSummary,
        txType,
        timestamp,
        ListMap(signatures: _*),
        newBoxes,
        data,
        from,
        minting,
        txId,
        boxesToRemove,
        fee,
        to,
        propositionType
      )
    )

  implicit val blockDataModelArbitrary: Arbitrary[BlockDataModel] =
    Arbitrary(
      for {
        id              <- Gen.asciiStr
        parentId        <- Gen.asciiStr
        timestamp       <- Gen.posNum[BigInt].map(_.toString())
        generatorBox    <- tokenBoxDataModelArbitrary.arbitrary
        publicKey       <- Gen.asciiStr
        signature       <- Gen.asciiStr
        height          <- Gen.posNum[Long]
        difficulty      <- Gen.asciiStr
        txRoot          <- Gen.asciiStr
        bloomFilter     <- Gen.asciiStr
        version         <- Gen.posNum[Int]
        numTransactions <- Gen.posNum[Int]
        blockSize       <- Gen.posNum[Int]
        fees            <- Gen.posNum[BigInt].map(_.toString())
      } yield BlockDataModel(
        id,
        parentId,
        timestamp,
        generatorBox,
        publicKey,
        signature,
        height,
        difficulty,
        txRoot,
        bloomFilter,
        version,
        numTransactions,
        blockSize,
        fees
      )
    )
}
