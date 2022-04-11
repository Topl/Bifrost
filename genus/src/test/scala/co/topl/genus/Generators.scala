package co.topl.genus

import co.topl.genus.filters._
import org.scalacheck.Gen

object Generators {

  val stringSelectionGen: Gen[StringSelection] = Gen.listOf(Gen.asciiStr).map(StringSelection.of)

  val minNumberRangeFilterTypeGen: Gen[NumberRange.FilterType.Min] =
    Gen.chooseNum(Long.MinValue, Long.MaxValue).map(NumberRange.FilterType.Min.apply)

  val maxNumberRangeFilterTypeGen: Gen[NumberRange.FilterType.Max] =
    Gen.chooseNum(Long.MinValue, Long.MaxValue).map(NumberRange.FilterType.Max.apply)

  val numberRangeFilterTypeGen: Gen[NumberRange.FilterType] =
    Gen.oneOf(
      minNumberRangeFilterTypeGen,
      maxNumberRangeFilterTypeGen,
      Gen.const(NumberRange.FilterType.Empty)
    )

  val numberRangeGen: Gen[NumberRange] = numberRangeFilterTypeGen.map(NumberRange.of)

  val numberSelectionGen: Gen[NumberSelection] =
    Gen.listOf(Gen.chooseNum(Long.MinValue, Long.MaxValue)).map(NumberSelection.of)

  val booleanSelectionGen: Gen[BooleanSelection] =
    Gen.oneOf(true, false).map(BooleanSelection.of)

  val tokenValueFilterGen: Gen[TokenValueFilter] =
    Gen
      .oneOf(
        stringSelectionGen.map(TokenValueFilter.FilterType.AssetCodeSelection.apply),
        numberRangeGen.map(TokenValueFilter.FilterType.QuantityRange.apply),
        stringSelectionGen.map(TokenValueFilter.FilterType.TokenValueTypeSelection.apply)
      )
      .map(TokenValueFilter.of)

  val transactionFilterGen: Gen[TransactionFilter] =
    Gen
      .oneOf(
        stringSelectionGen
          .map(TransactionFilter.FilterType.InputAddressSelection.apply),
        numberRangeGen
          .map(TransactionFilter.FilterType.TimestampRange.apply),
        stringSelectionGen
          .map(TransactionFilter.FilterType.InputAddressSelection.apply),
        numberSelectionGen
          .map(TransactionFilter.FilterType.InputNonceSelection.apply),
        stringSelectionGen
          .map(TransactionFilter.FilterType.OutputTokenBoxTypeSelection.apply),
        tokenValueFilterGen
          .map(TransactionFilter.FilterType.OutputTokenValueFilter.apply),
        stringSelectionGen
          .map(TransactionFilter.FilterType.OutputAddressSelection.apply),
        booleanSelectionGen
          .map(TransactionFilter.FilterType.MintingSelection.apply),
        stringSelectionGen
          .map(TransactionFilter.FilterType.TxIdSelection.apply),
        stringSelectionGen
          .map(TransactionFilter.FilterType.BoxesToRemoveSelection.apply),
        numberRangeGen
          .map(TransactionFilter.FilterType.FeeRange.apply),
        Gen.const(TransactionFilter.FilterType.All.apply(TransactionFilter.AllFilter.of()))
      )
      .map(TransactionFilter.of)

  val blockFilterGen: Gen[BlockFilter] =
    Gen
      .oneOf(
        stringSelectionGen
          .map(BlockFilter.FilterType.IdSelection.apply),
        stringSelectionGen
          .map(BlockFilter.FilterType.ParentIdSelection.apply),
        numberRangeGen
          .map(BlockFilter.FilterType.TimestampRange.apply),
        tokenValueFilterGen
          .map(BlockFilter.FilterType.GeneratorBoxTokenValueFilter.apply),
        stringSelectionGen
          .map(BlockFilter.FilterType.PublicKeySelection.apply),
        numberRangeGen
          .map(BlockFilter.FilterType.HeightRange.apply),
        numberSelectionGen
          .map(BlockFilter.FilterType.HeightSelection.apply),
        numberRangeGen
          .map(BlockFilter.FilterType.DifficultyRange.apply),
        numberSelectionGen
          .map(BlockFilter.FilterType.VersionSelection.apply),
        numberRangeGen
          .map(BlockFilter.FilterType.NumTransactionRange.apply)
      )
      .map(BlockFilter.of)
}
