package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.effect.Async
import cats.implicits._
import co.topl.genus.algebras.MongoSubscription
import co.topl.genus.filters.{NumberRange, TransactionFilter}
import co.topl.genus.services.transactions_query.TransactionSorting
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.BlockHeight
import co.topl.utils.mongodb.DocumentDecoder
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.ConfirmedTransactionDataModel
import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.model.{Aggregates, Filters}
import org.mongodb.scala.{Document, MongoCollection}

import scala.util.Try

object ConfirmedTransactionDataMongoSubscription {

  def make[F[_]: Async: MonadThrow](
    collection: MongoCollection[Document]
  ): MongoSubscription[F, ConfirmedTransactionDataModel] =
    new Impl(collection)

  private class Impl[F[_]: Async: MonadThrow](collection: MongoCollection[Document])
      extends MongoSubscription[F, ConfirmedTransactionDataModel] {

    override def fromBlockHeight[Filter: MongoFilter](
      filter:      Filter,
      blockHeight: BlockHeight
    ): F[Source[ConfirmedTransactionDataModel, NotUsed]] =
      for {
        catchupData <-
          MonadThrow[F].catchNonFatal(
            Source.fromPublisher(
              collection
                .find(
                  Filters.and(
                    filter.toBsonFilter,
                    TransactionFilter(
                      TransactionFilter.FilterType.BlockHeightRange(
                        NumberRange(
                          NumberRange.FilterType.Min(blockHeight.value)
                        )
                      )
                    ).toBsonFilter
                  )
                )
                // sort by height ascending
                .sort(TransactionSorting(TransactionSorting.SortBy.Height(TransactionSorting.Height())).toBsonSorting)
            )
          )
        lastDocTimestampSource =
          catchupData.fold(none[BsonTimestamp]) { case (_, doc) =>
            doc.get("ts").flatMap(ts => Try(ts.asTimestamp()).toOption)
          }
        freshData =
          lastDocTimestampSource.flatMapConcat(opt =>
            opt.fold(Source.empty[Document])(timestamp =>
              Source
                .fromPublisher(
                  collection
                    .watch(Seq(Aggregates.filter(filter.toBsonFilter)))
                    .startAtOperationTime(timestamp)
                )
                .map(_.getFullDocument)
            )
          )
        documents = catchupData.mergePreferred(freshData, priority = false)
        parsed =
          documents
            .flatMapConcat(doc =>
              DocumentDecoder[ConfirmedTransactionDataModel]
                .fromDocument(doc)
                .fold(_ => Source.empty, Source.single)
            )
      } yield parsed
  }
}
