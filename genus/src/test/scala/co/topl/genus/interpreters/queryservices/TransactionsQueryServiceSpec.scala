package co.topl.genus.interpreters.queryservices

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.testkit.TestKit
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.services_types.Paging
import co.topl.genus.services.transactions_query.{QueryTxsReq, TransactionsQuery}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Transaction
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class TransactionsQueryServiceSpec
    extends AsyncFlatSpec
    with AsyncMockFactory
    with BeforeAndAfterAll
    with Matchers
    with ScalaFutures {
  behavior of "TransactionQueryService.Eval.make query"

  val system: ActorSystem = ActorSystem("test")
  implicit val materializer: Materializer = Materializer(system)

  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  it should "return an empty list of transactions when the database is empty" in {
    val queryFilter = TransactionFilter(TransactionFilter.FilterType.All(TransactionFilter.AllFilter()))
    val queryTxReq: QueryTxsReq = QueryTxsReq(queryFilter.some)

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryTransactions _)
      .expects(*, *)
      .returns(Source.empty.pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 5.seconds)

    val queryResult = underTest.query(queryTxReq)

    queryResult.futureValue.result.success.get.transactions should be(empty)
  }

  it should "return all existing transactions when the database is not empty and no filters are provided" in {
    val queryTxReq: QueryTxsReq = QueryTxsReq(None)
    val existingTxs: Seq[Transaction] = Seq(
      Transaction(data = "test1"),
      Transaction(data = "test2"),
      Transaction(data = "test3")
    )

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryTransactions _)
      .expects(*, *)
      .returns(Source(existingTxs).pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 5.seconds)

    val queryResult = underTest.query(queryTxReq)

    queryResult.futureValue.result.success.get.transactions shouldNot be(empty)
  }

  it should "return timeout failure when the query takes longer than the configured time to evaluate" in {
    val queryTxReq: QueryTxsReq = QueryTxsReq(None)

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryTransactions _)
      .expects(*, *)
      .returns(Source.never.pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 1.millis)

    val queryResult = underTest.query(queryTxReq)

    queryResult.futureValue.result.failure.get.reason.isQueryTimeout shouldBe true
  }

  it should "return data store error failure when the database client has an error" in {
    val queryTxReq: QueryTxsReq = QueryTxsReq(None)
    val errorMessage = "error occurred with connection to database!"

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] =
      mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryTransactions _)
      .expects(*, *)
      .returns(IO.fromEither(Left(new Throwable(errorMessage))))

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 1.seconds)

    val queryResult = underTest.query(queryTxReq)

    queryResult.futureValue.result.failure.get.reason.dataStoreConnectionError.get shouldBe errorMessage
  }

  it should "provide the database client with correct paging options" in {
    val paging = Some(Paging(0, 5))

    val queryTxsReq: QueryTxsReq =
      QueryTxsReq(None, 0, paging)

    val resultTxs = List(Transaction(data = "test"))

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] =
      mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]

    (databaseClient.queryTransactions _)
      .expects(*, paging)
      .returns(Source(resultTxs).pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 1.second)

    val result = underTest.query(queryTxsReq)

    result.futureValue.result.success.get.transactions shouldBe resultTxs
  }

  it should "fail with an error when paging options are invalid" in {
    val paging = Some(Paging(-1, -1))

    val queryTxsReq: QueryTxsReq =
      QueryTxsReq(None, 0, paging)

    val resultTxs = List()

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] =
      mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]

    (databaseClient.queryTransactions _)
      .expects(*, *)
      .never
      .returns(Source(resultTxs).pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 1.second)

    val result = underTest.query(queryTxsReq)

    result.futureValue.result.failure.get.reason.isInvalidQuery shouldBe true
  }

  it should "fail with an error when confirmation depth is negative" in {
    val confirmationDepth = -1;

    val queryTxsReq: QueryTxsReq =
      QueryTxsReq(None, confirmationDepth, None)

    val resultTxs = List()

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] =
      mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]

    (databaseClient.queryTransactions _)
      .expects(*, *)
      .never
      .returns(Source(resultTxs).pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 1.second)

    val result = underTest.query(queryTxsReq)

    result.futureValue.result.failure.get.reason.isInvalidQuery shouldBe true
  }
}
