package co.topl.genus.interpreters

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.testkit.scaladsl.TestSink
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.ops.implicits._
import co.topl.genus.services.transactions_query.TransactionSorting
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.BlockHeight
import org.mongodb.scala.Document
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class BatchedMongoSubscriptionSpec extends ScalaTestWithActorTestKit with AsyncFlatSpecLike with Matchers {

  behavior of "Batched Mongo Subscription"

  it should "iterate through all the existing values in the store" in {
    val mongoDocuments = List(Document("{ \"id\": 1 }"), Document("{ \"id\": 2}"), Document("{ \"id\": 3 }"))

    // filter, sorting, and confirmation depth will be unused
    val filter = TransactionFilter()
    val sort = TransactionSorting()
    val confirmationDepth = 1

    val batchSize = 1
    val batchSleepTime = 10.milliseconds

    val mongoStore = MockMongoStore.inMemoryWithoutSortingOrFiltering[IO](mongoDocuments)

    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.withHeight(BlockHeight(10))

    val underTest = BatchedMongoSubscription.make[IO](batchSize, batchSleepTime, mongoStore)

    val testSink = TestSink[Document]()

    (for {
      result <- underTest.create(filter, sort, confirmationDepth)
      probe = result.runWith(testSink)
      _ = probe.request(3)
      firstDocument = probe.expectNext()
      secondDocument = probe.expectNext()
      thirdDocument = probe.expectNext()
    } yield List(firstDocument, secondDocument, thirdDocument) shouldBe mongoDocuments)
      .mapFunctor[Future]
  }

  it should "only emit the batch size number of elements at a time" in {
    val mongoDocuments = List(Document("{ \"id\": 1 }"), Document("{ \"id\": 2}"), Document("{ \"id\": 3 }"))

    // filter, sorting, and confirmation depth will be unused
    val filter = TransactionFilter()
    val sort = TransactionSorting()
    val confirmationDepth = 1

    val batchSize = 2
    // wait a long time before batches
    val batchSleepTime = 100.seconds

    val mongoStore = MockMongoStore.inMemoryWithoutSortingOrFiltering[IO](mongoDocuments)

    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.withHeight(BlockHeight(10))

    val underTest = BatchedMongoSubscription.make[IO](batchSize, batchSleepTime, mongoStore)

    val testSink = TestSink[Document]()

    (for {
      result <- underTest.create(filter, sort, confirmationDepth)
      probe = result.runWith(testSink)
      _ = probe.request(3)
      batch = probe.expectNextN(2)
      _ = probe.expectNoMessage()
    } yield batch shouldBe mongoDocuments.take(2))
      .mapFunctor[Future]
  }

  it should "not emit any elements once all values in the store have been retrieved" in {
    val mongoDocuments = List(Document("{ \"id\": 1 }"), Document("{ \"id\": 2}"), Document("{ \"id\": 3 }"))

    // filter, sorting, and confirmation depth will be unused
    val filter = TransactionFilter()
    val sort = TransactionSorting()
    val confirmationDepth = 1

    val batchSize = 1
    val batchSleepTime = 10.milliseconds

    val mongoStore = MockMongoStore.inMemoryWithoutSortingOrFiltering[IO](mongoDocuments)

    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.withHeight(BlockHeight(10))

    val underTest = BatchedMongoSubscription.make[IO](batchSize, batchSleepTime, mongoStore)

    val testSink = TestSink[Document]()

    (for {
      result <- underTest.create(filter, sort, confirmationDepth)
      probe = result.runWith(testSink)
      _ = probe.request(4)
      storeValues = probe.expectNextN(3)
      _ = probe.expectNoMessage()
    } yield storeValues shouldBe mongoDocuments)
      .mapFunctor[Future]
  }
}
