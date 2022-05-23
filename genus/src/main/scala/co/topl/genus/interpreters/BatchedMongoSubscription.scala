package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.data.OptionT
import cats.implicits._
import cats.{~>, Applicative, Functor}
import co.topl.genus.algebras.{ChainHeight, MongoStore, MongoSubscription}
import co.topl.genus.flows.FutureOptionTFilterFlow
import co.topl.genus.ops.implicits._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import co.topl.genus.types.BlockHeight
import org.mongodb.scala.Document

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
              .via(FutureOptionTFilterFlow.create(whenConfirmed(confirmationDepth, documentToHeightOpt, chainHeight)))
              .runWith(Sink.seq[Document])
              // increment the current index in the stream of documents for the next batch
              .map(documents => (index + documents.length -> documents.toList).some)
          )
          // TODO variable batching times (look into 'delayWith' function)
          .throttle(1, batchSleepTime)
          .mapConcat(values => values)
          .pure[F]
    }

  /**
   * Checks if the given document is confirmed for the given confirmation depth.
   * @param confirmationDepth the confirmation depth to use for checking if given documents have been confirmed
   * @param document the document to check
   * @return if cofnirmed, a Some option-T value, otherwise a None
   */
  private[genus] def whenConfirmed[F[_]: Functor](
    confirmationDepth:   Int,
    documentToHeightOpt: Document => Option[BlockHeight],
    chainHeight:         ChainHeight[F]
  )(
    document: Document
  ): OptionT[F, Document] =
    OptionT(
      chainHeight.get.map(currentHeight =>
        documentToHeightOpt(document)
          .filter(docHeight => docHeight.value <= currentHeight.value - confirmationDepth)
          .as(document)
      )
    )
}
