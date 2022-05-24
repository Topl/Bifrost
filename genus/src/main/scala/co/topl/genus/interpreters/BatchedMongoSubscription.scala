package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.implicits._
import cats.{~>, Monad}
import co.topl.genus.algebras.{ChainHeight, MongoStore, MongoSubscription}
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort, WithMaxBlockHeight}
import co.topl.genus.types.BlockHeight
import org.mongodb.scala.Document

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

object BatchedMongoSubscription {

  def make[F[_]: Monad: *[_] ~> Future: ChainHeight](
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
            Source
              .asyncF(
                filter
                  .withConfirmationDepth[F](confirmationDepth)
                  .flatMap(heightFilter =>
                    store
                      .getDocuments(heightFilter.toBsonFilter.some, sort.toBsonSorting.some, batchSize.some, index.some)
                  )
              )
              .flatMapConcat(identity)
              .runWith(Sink.seq[Document])
              // increment the current index in the stream of documents for the next batch
              .map(documents => (index + documents.length -> documents.toList).some)
          )
          // TODO variable batching times (look into 'delayWith' function)
          .throttle(1, batchSleepTime)
          .mapConcat(values => values)
          .pure[F]
    }

  private[genus] def getBatchFromIndex[F[_]: Monad, Filter: MongoFilter: WithMaxBlockHeight, Sort: MongoSort](
    filter:            Filter,
    sort:              Sort,
    confirmationDepth: Int,
    fromIndex:         Int,
    batchSize:         Int,
    chainHeight:       ChainHeight[F],
    store:             MongoStore[F]
  ): F[Source[Document, NotUsed]] =
    for {
      currentHeight <- chainHeight.get
      maxBlockHeight = BlockHeight(currentHeight.value - confirmationDepth)
      filterWithMaxBlockHeight = filter.withMaxBlockHeight(maxBlockHeight)
      documents <-
        store.getDocuments(
          filterWithMaxBlockHeight.toBsonFilter.some,
          sort.toBsonSorting.some,
          batchSize.some,
          fromIndex.some
        )
    } yield documents
}
