package co.topl.genus.typeclasses

import co.topl.genus.filters._
import org.mongodb.scala.model.Filters

trait MongoFilterInstances {

  implicit val transactionMongoFilter: MongoFilter[TransactionFilter] = filter =>
    filter.filterType match {

      case TransactionFilter.FilterType.TxTypeSelection(selection) =>
        stringSelection("txType").toFilter(selection)

      case TransactionFilter.FilterType.TimestampRange(range) =>
        numberRangeAsString("timestamp").toFilter(range)

      case TransactionFilter.FilterType.InputAddressSelection(selection) =>
        stringSelection("from.0").toFilter(selection)

      case TransactionFilter.FilterType.InputNonceSelection(selection) =>
        numberSelection("from.1").toFilter(selection)

      case TransactionFilter.FilterType.OutputTokenBoxTypeSelection(selection) =>
        stringSelection("newBoxes.type").toFilter(selection)

      case TransactionFilter.FilterType.OutputTokenValueFilter(tokenValueFilter) =>
        Filters.elemMatch("to", tokenValue("1").toFilter(tokenValueFilter))

      case TransactionFilter.FilterType.OutputAddressSelection(selection) =>
        Filters.elemMatch("to", stringSelection("0").toFilter(selection))

      case TransactionFilter.FilterType.MintingSelection(selection) =>
        Filters.eq("minting", selection.value)

      case TransactionFilter.FilterType.TxIdSelection(selection) =>
        stringSelection("txId").toFilter(selection)

      case TransactionFilter.FilterType.BoxesToRemoveSelection(selection) =>
        stringSelection("boxesToRemove").toFilter(selection)

      case TransactionFilter.FilterType.FeeRange(range) =>
        numberRangeAsString("fee").toFilter(range)

      case TransactionFilter.FilterType.PropositionSelection(selection) =>
        stringSelection("propositionType").toFilter(selection)

      case TransactionFilter.FilterType.BlockIdSelection(selection) =>
        stringSelection("block.id").toFilter(selection)

      case TransactionFilter.FilterType.BlockHeightRange(range) =>
        numberRange("block.height").toFilter(range)

      case TransactionFilter.FilterType.And(and) =>
        Filters.and(
          and.filters.map(transactionMongoFilter.toFilter): _*
        )

      case TransactionFilter.FilterType.Or(or) =>
        Filters.or(
          or.filters.map(transactionMongoFilter.toFilter): _*
        )

      case TransactionFilter.FilterType.Not(not) =>
        Filters.not(not.filter.map(transactionMongoFilter.toFilter).getOrElse(Filters.empty()))

      case _ => Filters.empty()
    }

  implicit val blockMongoFilter: MongoFilter[BlockFilter] = filter =>
    filter.filterType match {

      case BlockFilter.FilterType.IdSelection(selection) =>
        stringSelection("id").toFilter(selection)

      case BlockFilter.FilterType.ParentIdSelection(selection) =>
        stringSelection("parentId").toFilter(selection)

      case BlockFilter.FilterType.TimestampRange(range) =>
        numberRangeAsString("timestamp").toFilter(range)

      case BlockFilter.FilterType.GeneratorBoxTokenValueFilter(tokenValueFilter) =>
        tokenValue("generatorBox").toFilter(tokenValueFilter)

      case BlockFilter.FilterType.PublicKeySelection(selection) =>
        stringSelection("publicKey").toFilter(selection)

      case BlockFilter.FilterType.HeightRange(range) =>
        numberRange("height").toFilter(range)

      case BlockFilter.FilterType.DifficultyRange(range) =>
        numberRangeAsString("difficulty").toFilter(range)

      case BlockFilter.FilterType.VersionSelection(range) =>
        numberSelection("version").toFilter(range)

      case BlockFilter.FilterType.NumTransactionRange(range) =>
        numberRange("numTransactions").toFilter(range)

      case BlockFilter.FilterType.And(and) =>
        Filters.and(
          and.filters.map(blockMongoFilter.toFilter): _*
        )

      case BlockFilter.FilterType.Or(or) =>
        Filters.or(
          or.filters.map(blockMongoFilter.toFilter): _*
        )

      case BlockFilter.FilterType.Not(not) =>
        Filters.not(not.filter.map(blockMongoFilter.toFilter).getOrElse(Filters.empty()))

      case _ => Filters.empty()
    }

  private def stringSelection(field: String): MongoFilter[StringSelection] = filter =>
    Filters.in(field, filter.values: _*)

  private def numberSelection(field: String): MongoFilter[NumberSelection] = filter =>
    Filters.in(field, filter.values: _*)

  private def numberRange(property: String): MongoFilter[NumberRange] = {
    case NumberRange(NumberRange.FilterType.Min(min), _) => Filters.gte(property, min)
    case NumberRange(NumberRange.FilterType.Max(max), _) => Filters.lte(property, max)
    case _                                               => Filters.empty()
  }

  private def numberRangeAsString(field: String): MongoFilter[NumberRange] = {
    case NumberRange(NumberRange.FilterType.Min(min), _) => Filters.gte(field, min.toString)
    case NumberRange(NumberRange.FilterType.Max(max), _) => Filters.lte(field, max.toString)
    case _                                               => Filters.empty()
  }

  private def tokenValue(path: String): MongoFilter[TokenValueFilter] = {
    case TokenValueFilter(TokenValueFilter.FilterType.AssetCodeSelection(select), _) =>
      stringSelection(path + ".assetCode").toFilter(select)
    case TokenValueFilter(TokenValueFilter.FilterType.QuantityRange(range), _) =>
      numberRangeAsString(path + ".quantity").toFilter(range)
    case TokenValueFilter(TokenValueFilter.FilterType.TokenValueTypeSelection(select), _) =>
      stringSelection(path + ".type").toFilter(select)
    case _ => Filters.empty()
  }
}
