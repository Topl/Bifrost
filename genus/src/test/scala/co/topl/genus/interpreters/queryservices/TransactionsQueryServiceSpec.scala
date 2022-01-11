package co.topl.genus.interpreters.queryservices

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.services.transactions_query.{QueryTxsReq, TransactionsQuery}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Transaction
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class TransactionsQueryServiceSpec extends AsyncFlatSpec with AsyncMockFactory with Matchers {
  behavior of "TransactionQueryService.Eval.make query"

  val actorSystem: ActorSystem = ActorSystem("test")
  implicit val materializer: Materializer = Materializer(actorSystem)

  it should "return an empty list of transactions when the database is empty" in {
    val queryFilter = TransactionFilter(TransactionFilter.FilterType.All(TransactionFilter.AllFilter()))
    val queryTxReq: QueryTxsReq = QueryTxsReq(queryFilter.some)

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryTransactions _)
      .expects(*)
      .returns(Source.empty.pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 5.seconds)

    val result = underTest.query(queryTxReq)

    result map { r => r.result.success.get.transactions should be(empty) }
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
      .expects(*)
      .returns(Source(existingTxs).pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 5.seconds)

    val result = underTest.query(queryTxReq)

    result map { r => r.result.success.get.transactions shouldNot be(empty) }
  }

  it should "return timeout failure when the query takes longer than the configured time to evaluate" in {
    val queryTxReq: QueryTxsReq = QueryTxsReq(None)

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryTransactions _)
      .expects(*)
      .returns(Source.never.pure[IO])

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 1.seconds)

    val result = underTest.query(queryTxReq)

    result map { r =>
      r.result.failure.get.reason.isQueryTimeout shouldBe true
    }
  }

  it should "return data store error failure when the database client has an error" in {
    val queryTxReq: QueryTxsReq = QueryTxsReq(None)
    val errorMessage = "error occurred with connection to database!"

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] =
      mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryTransactions _)
      .expects(*)
      .returns(IO.fromEither(Left(new Throwable(errorMessage))))

    val underTest: TransactionsQuery =
      TransactionsQueryService.Eval.make(databaseClient, 1.seconds)

    val result = underTest.query(queryTxReq)

    result map { r =>
      r.result.failure.get.reason.dataStoreConnectionError.get shouldBe errorMessage
    }
  }
}
