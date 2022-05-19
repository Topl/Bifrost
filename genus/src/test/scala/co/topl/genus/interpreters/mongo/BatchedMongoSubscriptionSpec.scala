package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.implicits._
import co.topl.genus.Generators
import co.topl.genus.algebras.{ChainHeight, MongoStore}
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.interpreters.BatchedMongoSubscription
import co.topl.genus.services.transactions_query.TransactionSorting
import co.topl.genus.typeclasses.implicits._
import org.mongodb.scala.Document
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.genus.ops.implicits._
import co.topl.genus.types.BlockHeight

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class BatchedMongoSubscriptionSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with ScalaFutures
    with MockFactory
    with ScalaCheckDrivenPropertyChecks
    with EitherValues {

  import BatchedMongoSubscriptionSpec._

  implicit val executionContext: ExecutionContext = system.executionContext
  import cats.effect.unsafe.implicits.global

  behavior of "Batched Mongo Subscription Spec"

  it should "retrieve all documents previously existing in storage" in {
    forAll(Gen.listOf(Generators.confirmedTransactionDataModelDocumentGen)) { documents =>
      // batch size and sleep time will be ignored since we will just return the list of generated documents
      val batchSize = 100
      val batchSleepTime = 10.millis

      val mongoStoreMock: MongoStore[IO] = mock[MongoStore[IO]]
      (mongoStoreMock.getDocuments _)
        .expects(*, *, *, *)
        .anyNumberOfTimes()
        .returns(Source(documents).mapMaterializedValue(_ => NotUsed).pure[IO])

      val chainHeightMock: ChainHeight[IO] = mock[ChainHeight[IO]]
      (() => chainHeightMock.get)
        .expects()
        .anyNumberOfTimes()
        // guarantee that confirmation depth will never filter out any values
        .returns(IO(BlockHeight(Long.MaxValue)))

      val underTest =
        BatchedMongoSubscription
          .make[IO](batchSize, batchSleepTime, _.getTransactionBlockHeight, mongoStoreMock, chainHeightMock)

      val resultsSource = underTest.create(TransactionFilter.defaultInstance, TransactionSorting.defaultInstance, 0)
      val resultsList = sourceIOToList(resultsSource.map(_.take(documents.length))).futureValue

      resultsList shouldBe documents
    }
  }

  it should "never complete upstream" in {
    // batch size and sleep time will be ignored since we will just return the list of generated documents
    val batchSize = 100
    val batchSleepTime = 10.millis

    val mongoStoreMock: MongoStore[IO] = mock[MongoStore[IO]]
    (mongoStoreMock.getDocuments _)
      .expects(*, *, *, *)
      .anyNumberOfTimes()
      .returns(Source.empty[Document].mapMaterializedValue(_ => NotUsed).pure[IO])

    val chainHeightMock: ChainHeight[IO] = mock[ChainHeight[IO]]
    (() => chainHeightMock.get)
      .expects()
      .anyNumberOfTimes()
      // guarantee that confirmation depth will never filter out any values
      .returns(IO(BlockHeight(Long.MaxValue)))

    val underTest = BatchedMongoSubscription
      .make[IO](batchSize, batchSleepTime, _.getTransactionBlockHeight, mongoStoreMock, chainHeightMock)

    val resultSource = underTest
      .create(TransactionFilter.defaultInstance, TransactionSorting.defaultInstance, 0)

    val result =
      resultSource
        .map(_.completionTimeout(1.second))
        .flatMap(x => IO.fromFuture(x.runWith(Sink.seq[Document]).pure[IO]))
        .map(_.asRight[Throwable])
        .handleError(failure => failure.asLeft)
        .unsafeToFuture()
        .futureValue

    result.leftMap(_.getMessage) shouldBe Left("The stream has not been completed in 1 second.")
  }
}

object BatchedMongoSubscriptionSpec {

  /**
   * Converts the given source into a Future value of a list of elements provided by the source.
   * @param sourceIO the source to aggregate elements from
   * @param ioRuntime an implicit IO runtime
   * @param materializer an implicit akka materializer
   * @param executionContext an implicit execution context
   * @tparam T the type of the elements
   * @tparam Mat the materialized type of the source
   * @return a future containing a list of elements emitted by the source
   */
  def sourceIOToList[T, Mat](
    sourceIO:           IO[Source[T, Mat]]
  )(implicit ioRuntime: IORuntime, materializer: Materializer, executionContext: ExecutionContext): Future[List[T]] =
    sourceIO
      .map(source => source.runWith(Sink.seq[T]))
      .unsafeToFuture()
      .flatten
      .map(_.toList)
}
