package co.topl.genus.interpreters

import cats.implicits._
import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.{~>, Applicative}
import co.topl.genus.algebras.{ChainHeight, MongoStore, MongoSubscription}
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.mongodb.scala.Document
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.BlockHeight

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

object BatchedMongoSubscription {

  def make[F[_]: Applicative: *[_] ~> Future](
    batchSize:           Int,
    batchSleepTime:      FiniteDuration,
    documentToHeightOpt: Document => Option[BlockHeight],
    store:               MongoStore[F],
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
              .futureSource(
                store
                  .getDocuments(filter.toBsonFilter.some, sort.toBsonSorting.some, batchSize.some, index.some)
                  .mapFunctor[Future]
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
