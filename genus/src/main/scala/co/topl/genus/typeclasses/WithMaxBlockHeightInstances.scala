package co.topl.genus.typeclasses

import co.topl.genus.filters.{BlockFilter, NumberRange, TransactionFilter}
import co.topl.genus.types.BlockHeight

trait WithMaxBlockHeightInstances {

  implicit val transactionWithMaxBlockHeight: WithMaxBlockHeight[TransactionFilter] =
    (value: TransactionFilter, height: BlockHeight) =>
      TransactionFilter(
        TransactionFilter.FilterType.And(
          TransactionFilter.AndFilter(
            Seq(
              value,
              TransactionFilter(
                TransactionFilter.FilterType.BlockHeightRange(NumberRange(NumberRange.FilterType.Max(height.value)))
              )
            )
          )
        )
      )

  implicit val blockFilterWithMaxBlockHeight: WithMaxBlockHeight[BlockFilter] =
    (value: BlockFilter, height: BlockHeight) =>
      BlockFilter(
        BlockFilter.FilterType.And(
          BlockFilter.AndFilter(
            Seq(
              value,
              BlockFilter(
                BlockFilter.FilterType.HeightRange(NumberRange(NumberRange.FilterType.Max(height.value)))
              )
            )
          )
        )
      )
}
