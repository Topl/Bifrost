package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.kernel.Async
import cats.implicits._
import cats.~>
import co.topl.genus.algebras.{ChainHeight, MongoStore, MongoSubscription}
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort, WithMaxBlockHeight}
import org.mongodb.scala.Document

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

object BatchedMongoSubscription {

  def make[F[_]: Async: *[_] ~> Future: ChainHeight](
    batchSize:      Int,
    batchSleepTime: FiniteDuration,
    store:          MongoStore[F]
  )(implicit
    materializer:     Materializer,
    executionContext: ExecutionContext
  ): MongoSubscription[F] =
    new MongoSubscription[F] {

      override def create[Filter: MongoFilter: WithMaxBlockHeight, Sort: MongoSort](
        filter:            Filter,
        sort:              Sort,
        confirmationDepth: Int
      ): F[Source[Document, NotUsed]] =
        Source
          .unfoldAsync(0)(index =>
            filter
              .withConfirmationDepth[F](confirmationDepth)
              .flatMap(heightFilter =>
                store
                  .getDocuments(heightFilter.toBsonFilter.some, sort.toBsonSorting.some, batchSize.some, index.some)
              )
              .flatMap(source =>
                Async[F].fromFuture(
                  Async[F].delay(
                    source.runWith(Sink.seq[Document])
                  )
                )
              )
              .map(documents => (index + documents.length -> documents.toList).some)
              .mapFunctor[Future]
          )
          // TODO variable batching times (look into 'delayWith' function)
          .throttle(1, batchSleepTime)
          .mapConcat(values => values)
          .pure[F]
    }
}
