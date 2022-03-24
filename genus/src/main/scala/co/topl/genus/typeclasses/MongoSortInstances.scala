package co.topl.genus.typeclasses

import co.topl.genus.services.blocks_query.BlockSorting
import co.topl.genus.services.transactions_query.TransactionSorting
import org.mongodb.scala.model.Sorts

trait MongoSortInstances {

  implicit val transactionSortingMongoSort: MongoSort[TransactionSorting] =
    sorting =>
      sorting.sortBy match {
        case TransactionSorting.SortBy.Height(TransactionSorting.Height(true, _)) =>
          Sorts.descending("block.height")
        case TransactionSorting.SortBy.Height(TransactionSorting.Height(false, _)) =>
          Sorts.ascending("block.height")
        case TransactionSorting.SortBy.Fee(TransactionSorting.Fee(true, _)) =>
          Sorts.descending("fee")
        case TransactionSorting.SortBy.Fee(TransactionSorting.Fee(false, _)) =>
          Sorts.ascending("fee")
        case TransactionSorting.SortBy.Timestamp(TransactionSorting.Timestamp(true, _)) =>
          Sorts.descending("timestamp")
        case TransactionSorting.SortBy.Timestamp(TransactionSorting.Timestamp(false, _)) =>
          Sorts.ascending("timestamp")
        // use block height for default
        case _ => Sorts.ascending("block.height")
      }

  implicit val blockSortingMongoSort: MongoSort[BlockSorting] =
    sorting =>
      sorting.sortBy match {
        case BlockSorting.SortBy.Height(BlockSorting.Height(true, _)) =>
          Sorts.descending("height")
        case BlockSorting.SortBy.Height(BlockSorting.Height(false, _)) =>
          Sorts.ascending("height")
        case BlockSorting.SortBy.Timestamp(BlockSorting.Timestamp(true, _)) =>
          Sorts.descending("timestamp")
        case BlockSorting.SortBy.Timestamp(BlockSorting.Timestamp(false, _)) =>
          Sorts.ascending("timestamp")
        case BlockSorting.SortBy.Difficulty(BlockSorting.Difficulty(true, _)) =>
          Sorts.descending("difficulty")
        case BlockSorting.SortBy.Difficulty(BlockSorting.Difficulty(false, _)) =>
          Sorts.ascending("difficulty")
        case BlockSorting.SortBy.NumberOfTransactions(BlockSorting.NumberOfTransactions(true, _)) =>
          Sorts.descending("numTransactions")
        case BlockSorting.SortBy.NumberOfTransactions(BlockSorting.NumberOfTransactions(false, _)) =>
          Sorts.ascending("numTransactions")
        // use height as default
        case _ => Sorts.ascending("height")
      }
}
