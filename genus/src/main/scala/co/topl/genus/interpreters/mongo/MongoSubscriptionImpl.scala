package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.Applicative
import cats.implicits._
import co.topl.genus.algebras.{ChainHeight, MongoSubscription}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import co.topl.genus.types.BlockHeight
import co.topl.genus.ops.implicits._
import org.mongodb.scala.{Document, MongoCollection}
import cats.~>

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

object MongoSubscriptionImpl {

  def make[F[_]: Applicative: *[_] ~> Future](
    batchSize:           Int,
    batchSleepTime:      FiniteDuration,
    documentToHeightOpt: Document => Option[BlockHeight],
    collection:          MongoCollection[Document],
    chainHeight:         ChainHeight[F]
  )(implicit
    materializer:     Materializer,
    executionContext: ExecutionContext
  ): MongoSubscription[F] =
    new MongoSubscription[F] {

      override def create[Filter: MongoFilter, Sort: MongoSort](
        filter:            Filter,
        sort:              Sort,
        confirmationDepth: Int
      ): F[Source[Document, NotUsed]] =
        Source
          .unfoldAsync(0)(index =>
            Source
              .fromPublisher(
                collection
                  .find(filter.toBsonFilter)
                  .sort(sort.toBsonSorting)
                  .skip(index)
                  .limit(batchSize)
              )
              .flatMapConcat(document =>
                Source
                  .asyncF(chainHeight.get)
                  .mapConcat(height =>
                    documentToHeightOpt(document)
                      .flatMap(documentHeight =>
                        if (documentHeight.value <= height.value - confirmationDepth) document.some
                        else None
                      )
                      .toList
                  )
              )
              .runWith(Sink.seq[Document])
              // increment the current index in the stream of documents for the next batch
              .map(documents => (index + documents.length -> documents.toList).some)
          )
          // TODO variable batching times (look into 'delayWith' function)
          .throttle(1, batchSleepTime)
          .mapConcat(values => values)
          .pure[F]
    }
}
