package co.topl.genus

import co.topl.genus.filters._
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.models._
import org.mongodb.scala.Document
import org.scalacheck.Gen

import scala.collection.immutable.ListMap

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

  val blockSummaryDataModelGen: Gen[BlockSummaryDataModel] =
    Gen.zip(Gen.asciiStr, Gen.posNum[Long]).map(values => BlockSummaryDataModel(values._1, values._2))

  val simpleValueDataModelGen: Gen[SimpleValueDataModel] =
    Gen.posNum[BigInt].map(value => SimpleValueDataModel(value.toString()))

  val assetValueDataModelGen: Gen[AssetValueDataModel] =
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

  val tokenValueDataModelGen: Gen[TokenValueDataModel] = Gen.oneOf(simpleValueDataModelGen, assetValueDataModelGen)

  val tokenBoxDataModelGen: Gen[TokenBoxDataModel] =
    Gen
      .zip(
        Gen.asciiStr,
        Gen.asciiStr,
        Gen.posNum[BigInt].map(_.toString()),
        Gen.asciiStr,
        tokenValueDataModelGen
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

  val confirmedTransactionDataModelGen: Gen[ConfirmedTransactionDataModel] =
    for {
      blockSummary    <- blockSummaryDataModelGen
      txType          <- Gen.oneOf(Seq("PolyTransfer", "ArbitTransfer", "AssetTransfer"))
      timestamp       <- Gen.posNum[BigInt].map(_.toString())
      signatures      <- Gen.listOf(Gen.zip(Gen.asciiStr, Gen.asciiStr))
      newBoxes        <- Gen.listOf(tokenBoxDataModelGen)
      data            <- Gen.option(Gen.asciiStr)
      from            <- Gen.listOf(Gen.zip(Gen.asciiStr, Gen.asciiStr))
      minting         <- Gen.oneOf(true, false)
      txId            <- Gen.asciiStr
      boxesToRemove   <- Gen.listOf(Gen.asciiStr)
      fee             <- Gen.posNum[BigInt].map(_.toString())
      to              <- Gen.listOf(Gen.zip(Gen.asciiStr, tokenValueDataModelGen))
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

  val blockDataModelGen: Gen[BlockDataModel] =
    for {
      id              <- Gen.asciiStr
      parentId        <- Gen.asciiStr
      timestamp       <- Gen.posNum[BigInt].map(_.toString())
      generatorBox    <- tokenBoxDataModelGen
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

  val confirmedTransactionDataModelDocumentGen: Gen[Document] = confirmedTransactionDataModelGen.map(_.asDocument)

  val blockDataModelDocumentGen: Gen[Document] = blockDataModelGen.map(_.asDocument)
}
