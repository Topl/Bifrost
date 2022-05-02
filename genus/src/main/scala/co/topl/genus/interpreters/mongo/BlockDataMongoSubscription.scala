package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.MonadThrow
import cats.effect.Async
import cats.implicits._
import co.topl.genus.algebras.MongoSubscription
import co.topl.genus.filters.{BlockFilter, NumberRange}
import co.topl.genus.services.blocks_query.BlockSorting
import co.topl.genus.typeclasses.MongoFilter
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.{Block, BlockHeight}
import co.topl.utils.mongodb.DocumentDecoder
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.BlockDataModel
import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.model.changestream.ChangeStreamDocument
import org.mongodb.scala.model.{Aggregates, Filters}
import org.mongodb.scala.{Document, MongoCollection}

import scala.util.Try

object BlockDataMongoSubscription {

  def make[F[_]: Async: MonadThrow](collection: MongoCollection[Document]): MongoSubscription[F, BlockDataModel] =
    new Impl(collection)

  private class Impl[F[_]: Async: MonadThrow](collection: MongoCollection[Document])
      extends MongoSubscription[F, BlockDataModel] {

    override def fromBlockHeight[Filter: MongoFilter](
      filter:      Filter,
      blockHeight: BlockHeight
    ): F[Source[BlockDataModel, NotUsed]] =
      for {
        catchupData <-
          MonadThrow[F].catchNonFatal(
            Source.fromPublisher(
              collection
                .find(
                  Filters.and(
                    filter.toBsonFilter,
                    BlockFilter(
                      BlockFilter.FilterType.HeightRange(
                        NumberRange(
                          NumberRange.FilterType.Min(blockHeight.value)
                        )
                      )
                    ).toBsonFilter
                  )
                )
                // sort by height ascending
                .sort(BlockSorting(BlockSorting.SortBy.Height(BlockSorting.Height())).toBsonSorting)
            )
          )
        lastDocTimestampSource =
          catchupData.fold(none[BsonTimestamp]) { case (_, doc) =>
            doc.get("ts").flatMap(ts => Try(ts.asTimestamp()).toOption)
          }
        freshData =
          lastDocTimestampSource.flatMapConcat(timestampOpt =>
            timestampOpt
              .fold(Source.empty[Document])(timestamp =>
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
              DocumentDecoder[BlockDataModel]
                .fromDocument(doc)
                .fold(_ => Source.empty, Source.single)
            )
      } yield parsed
  }
}
