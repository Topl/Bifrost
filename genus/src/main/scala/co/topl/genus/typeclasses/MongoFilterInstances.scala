package co.topl.genus.typeclasses

import co.topl.genus.filters._
import org.mongodb.scala.model.Filters

trait MongoFilterInstances {

  implicit val transactionMongoFilter: MongoFilter[TransactionFilter] = filter =>
    filter.filterType match {

      case TransactionFilter.FilterType.TxTypeSelection(select) =>
        stringSelection("txType").toFilter(select)

      case TransactionFilter.FilterType.TimestampRange(range) =>
        numberRangeAsString("timestamp").toFilter(range)

      case TransactionFilter.FilterType.InputBoxAddressSelection(select) =>
        stringSelection("from.0").toFilter(select)

      case TransactionFilter.FilterType.InputNonceSelection(select) =>
        numberSelection("from.1").toFilter(select)

      case TransactionFilter.FilterType.OutputTokenBoxTypeSelection(select) =>
        stringSelection("newBoxes.type").toFilter(select)

      case TransactionFilter.FilterType.OutputTokenValueFilter(tokenValueFilter) =>
        Filters.elemMatch("to", tokenValue("1").toFilter(tokenValueFilter))

      case TransactionFilter.FilterType.OutputAddress(select) =>
        Filters.elemMatch("to", stringSelection("0").toFilter(select))

      case TransactionFilter.FilterType.MintingSelection(select) =>
        booleanSelection("minting").toFilter(select)

      case TransactionFilter.FilterType.TxIdSelection(select) =>
        stringSelection("txId").toFilter(select)

      case TransactionFilter.FilterType.BoxesToRemoveSelection(select) =>
        stringSelection("boxesToRemove").toFilter(select)

      case TransactionFilter.FilterType.FeeRange(range) =>
        numberRangeAsString("fee").toFilter(range)

      case TransactionFilter.FilterType.PropositionSelection(select) =>
        stringSelection("propositionType").toFilter(select)

      case TransactionFilter.FilterType.BlockIdSelection(select) =>
        stringSelection("block.id").toFilter(select)

      case TransactionFilter.FilterType.BlockHeightRange(range) =>
        numberRange("block.height").toFilter(range)

      case TransactionFilter.FilterType.And(and) =>
        Filters.and(
          and.first.map(transactionMongoFilter.toFilter).getOrElse(Filters.empty()),
          and.second.map(transactionMongoFilter.toFilter).getOrElse(Filters.empty())
        )

      case TransactionFilter.FilterType.Or(or) =>
        Filters.or(
          or.first.map(transactionMongoFilter.toFilter).getOrElse(Filters.empty()),
          or.second.map(transactionMongoFilter.toFilter).getOrElse(Filters.empty())
        )

      case TransactionFilter.FilterType.Not(not) =>
        Filters.not(not.filter.map(transactionMongoFilter.toFilter).getOrElse(Filters.empty()))

      case _ => Filters.empty()
    }

  implicit val blockMongoFilter: MongoFilter[BlockFilter] = filter =>
    filter.filterType match {

      case BlockFilter.FilterType.IdSelection(select) =>
        stringSelection("id").toFilter(select)

      case BlockFilter.FilterType.ParentIdSelection(select) =>
        stringSelection("parentId").toFilter(select)

      case BlockFilter.FilterType.TimestampRange(range) =>
        numberRangeAsString("timestamp").toFilter(range)

      case BlockFilter.FilterType.GeneratorBoxTokenValueFilter(tokenValueFilter) =>
        tokenValue("generatorBox").toFilter(tokenValueFilter)

      case BlockFilter.FilterType.PublicKeySelection(select) =>
        stringSelection("publicKey").toFilter(select)

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
          and.first.map(blockMongoFilter.toFilter).getOrElse(Filters.empty()),
          and.second.map(blockMongoFilter.toFilter).getOrElse(Filters.empty())
        )

      case BlockFilter.FilterType.Or(or) =>
        Filters.or(
          or.first.map(blockMongoFilter.toFilter).getOrElse(Filters.empty()),
          or.second.map(blockMongoFilter.toFilter).getOrElse(Filters.empty())
        )

      case BlockFilter.FilterType.Not(not) =>
        Filters.not(not.filter.map(blockMongoFilter.toFilter).getOrElse(Filters.empty()))

      case _ => Filters.empty()
    }

  private def stringSelection(field: String): MongoFilter[StringSelection] = filter =>
    if (filter.inclusive) Filters.in(field, filter.values: _*)
    else Filters.nin(field, filter.values: _*)

  private def numberSelection(field: String): MongoFilter[NumberSelection] = filter =>
    if (filter.inclusive) Filters.in(field, filter.values: _*)
    else Filters.nin(field, filter.values: _*)

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

  private def booleanSelection(field: String): MongoFilter[BooleanSelection] = filter => Filters.eq(field, filter.value)

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
