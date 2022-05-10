package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.Applicative
import cats.implicits._
import co.topl.genus.algebras.MongoSubscription
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.mongodb.scala.{Document, MongoCollection}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object MongoSubscriptionImpl {

  def make[F[_]: Applicative](
    batchSize:      Int,
    batchSleepTime: FiniteDuration,
    collection:     MongoCollection[Document]
  )(implicit
    materializer:     Materializer,
    executionContext: ExecutionContext
  ): MongoSubscription[F] =
    new MongoSubscription[F] {

      override def create[Filter: MongoFilter, Sort: MongoSort](
        filter: Filter,
        sort:   Sort
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
