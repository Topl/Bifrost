package co.topl.genus.interpreters

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT
import cats.effect.{Async, IO}
import cats.effect.testing.scalatest.AsyncIOSpec
import co.topl.genus.algebras.QueryServiceAlg._
import co.topl.genus.algebras._
import co.topl.genus.services.services_types.Paging
import org.scalacheck.Gen
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Second, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

class QueryServiceInterpSpec
    extends AsyncFlatSpec
    with EitherValues
    with AsyncMockFactory
    with Matchers
    with BeforeAndAfterAll
    with AsyncIOSpec {

  implicit val system: ActorSystem = ActorSystem("QueryServiceInterpSpec")

  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  type F[T] = IO[T]

  behavior of "Query Service Interpreter asList"

  it should "fail with an error when paging options are negative" in {
    val pageNumber = -5;
    val pageSize = -10;

    val filter: String = "test-filter"
    val sort: Boolean = false
    val paging = Some(Paging(pageNumber, pageSize))
    val confirmationDepth = 0

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
    (dataStore.query _)
      .expects(*, *, *)
      .never()

    val underTest: QueryServiceAlg[F, String, String, Boolean] =
      QueryServiceInterp.Eval.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, List[String]] = underTest.asList(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }

  it should "fail with an error when confirmation depth is negative" in {
    val confirmationDepth = -10;
    val filter: String = "test-filter"
    val sort: Boolean = false
    val paging: Option[Paging] = None

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
    (dataStore.query _)
      .expects(*, *, *)
      .never()

    val underTest: QueryServiceAlg[F, String, String, Boolean] =
      QueryServiceInterp.Eval.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, List[String]] = underTest.asList(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }

  it should "use provided filter to query data store" in {
    val filter = "test-filter"
    val sort: Boolean = false
    val paging: Option[Paging] = None
    val confirmationDepth = 0

    val dataValues = List.empty[String]

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
    (dataStore.query _)
      .expects(filter, sort, *)
      .once()
      .returns(IO.pure(Source(dataValues)))

    val underTest: QueryServiceAlg[F, String, String, Boolean] =
      QueryServiceInterp.Eval.make(dataStore, 5.seconds)

    val result: EitherT[F, QueryFailure, List[String]] = underTest.asList(request)

    result.value.asserting(_.value shouldBe dataValues)
  }

  behavior of "Query Service Interpreter asSource"

  it should "fail with an error when paging options are negative" in {
    val pageNumber = -9;
    val pageSize = -100;
    val filter: String = "test-filter"
    val sort: Boolean = true
    val paging = Some(Paging(pageNumber, pageSize))
    val confirmationDepth = 0

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
    (dataStore.query _)
      .expects(*, *, *)
      .never()

    val underTest: QueryServiceAlg[F, String, String, Boolean] =
      QueryServiceInterp.Eval.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }

  it should "fail with an error when confirmation depth is negative" in {
    val confirmationDepth = -100;
    val filter: String = "test-filter"
    val sort: Boolean = true
    val paging: Option[Paging] = None

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
    (dataStore.query _)
      .expects(*, *, *)
      .never()

    val underTest: QueryServiceAlg[F, String, String, Boolean] =
      QueryServiceInterp.Eval.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }

  it should "use provided filter to query data store" in {
    val filter: String = "test-filter"
    val sort: Boolean = true
    val paging: Option[Paging] = None
    val confirmationDepth = 0

    val dataValues = List.empty[String]

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
    (dataStore.query _)
      .expects(filter, sort, *)
      .once()
      .returns(IO.pure(Source(dataValues)))

    val underTest: QueryServiceAlg[F, String, String, Boolean] =
      QueryServiceInterp.Eval.make(dataStore, 5.seconds)

    val result: EitherT[F, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

    toList(result).value.asserting(_.value shouldBe dataValues)
  }

  private def toList[F[_]: Async, Err, T, Mat](value: EitherT[F, Err, Source[T, Mat]]): EitherT[F, Err, List[T]] =
    value
      .flatMap(source =>
        EitherT.right[Err](
          Async[F].fromFuture(
            source
              .runWith(Sink.seq)
              .pure[F]
          )
        )
      )
      .map(_.toList)
}
