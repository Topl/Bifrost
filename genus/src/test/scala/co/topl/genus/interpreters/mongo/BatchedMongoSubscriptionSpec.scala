package co.topl.genus.interpreters.mongo

import akka.NotUsed
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.implicits._
import co.topl.genus.Generators
import co.topl.genus.algebras.MongoStore
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.interpreters.BatchedMongoSubscription
import co.topl.genus.services.transactions_query.TransactionSorting
import co.topl.genus.typeclasses.implicits._
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

class BatchedMongoSubscriptionSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with ScalaFutures
    with MockFactory
    with ScalaCheckDrivenPropertyChecks {

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

      val underTest = BatchedMongoSubscription.make[IO](batchSize, batchSleepTime, mongoStoreMock)

      val resultsSource = underTest.create(TransactionFilter.defaultInstance, TransactionSorting.defaultInstance)
      val resultsList = sourceIOToList(resultsSource.map(_.take(documents.length))).futureValue

      resultsList shouldBe documents
    }
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
