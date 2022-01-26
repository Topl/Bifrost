package co.topl.genus.interpreters

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.QueryServiceAlg._
import co.topl.genus.algebras._
import co.topl.genus.services.services_types.Paging
import org.scalacheck.Gen
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class QueryServiceInterpSpec
    extends AsyncFlatSpecLike
    with ScalaCheckPropertyChecks
    with ScalaFutures
    with EitherValues
    with AsyncMockFactory
    with Matchers
    with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("QueryServiceInterpSpec")

  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  type F[T] = IO[T]

  behavior of "Query Service Interpreter asList"

  it should "fail with an error when paging options are negative" in {
    forAll(Gen.negNum[Int], Gen.negNum[Int]) { (pageNumber, pageSize) =>
      whenever(pageNumber < 0 && pageSize < 0) {
        val filter: Option[String] = None
        val sort: Option[Boolean] = None
        val paging = Some(Paging(pageNumber, pageSize))
        val confirmationDepth = 0

        val request = QueryRequest(filter, sort, paging, confirmationDepth)

        val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
        (dataStore.query _)
          .expects(*, *, *)
          .never()

        val underTest: QueryServiceAlg[F, String, String, Boolean] =
          QueryServiceInterp.Eval.make(dataStore, "", false, 5.seconds)

        val result: EitherT[IO, QueryFailure, List[String]] = underTest.asList(request)

        result.value.unsafeToFuture().futureValue.left.value shouldBe a[QueryFailures.InvalidQuery[String, Boolean]]
      }
    }
  }

  it should "fail with an error when confirmation depth is negative" in {
    forAll(Gen.negNum[Int]) { (confirmationDepth) =>
      whenever(confirmationDepth < 0) {
        val filter: Option[String] = None
        val sort: Option[Boolean] = None
        val paging: Option[Paging] = None

        val request = QueryRequest(filter, sort, paging, confirmationDepth)

        val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
        (dataStore.query _)
          .expects(*, *, *)
          .never()

        val underTest: QueryServiceAlg[F, String, String, Boolean] =
          QueryServiceInterp.Eval.make(dataStore, "", false, 5.seconds)

        val result: EitherT[IO, QueryFailure, List[String]] = underTest.asList(request)

        result.value.unsafeToFuture().futureValue.left.value shouldBe a[QueryFailures.InvalidQuery[String, Boolean]]
      }
    }
  }

  it should "use default filter when no filter provided" in {
    forAll(Gen.asciiStr) { defaultFilter =>
      val filter: Option[String] = None
      val sort: Option[Boolean] = None
      val paging: Option[Paging] = None
      val confirmationDepth = 0

      val dataValues = List.empty[String]

      val defaultSort = true

      val request = QueryRequest(filter, sort, paging, confirmationDepth)

      val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
      (dataStore.query _)
        .expects(defaultFilter, defaultSort, *)
        .once()
        .returns(IO.pure(Source(dataValues)))

      val underTest: QueryServiceAlg[F, String, String, Boolean] =
        QueryServiceInterp.Eval.make(dataStore, defaultFilter, defaultSort, 5.seconds)

      val result: EitherT[F, QueryFailure, List[String]] = underTest.asList(request)

      result.value.unsafeToFuture().futureValue.value shouldBe dataValues
    }
  }

  it should "use provided filter to query data store" in {
    forAll(Gen.asciiStr) { filterValue =>
      val filter: Option[String] = Some(filterValue)
      val sort: Option[Boolean] = None
      val paging: Option[Paging] = None
      val confirmationDepth = 0

      val dataValues = List.empty[String]

      val defaultFilter = "test"
      val defaultSort = true

      val request = QueryRequest(filter, sort, paging, confirmationDepth)

      val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
      (dataStore.query _)
        .expects(filterValue, defaultSort, *)
        .once()
        .returns(IO.pure(Source(dataValues)))

      val underTest: QueryServiceAlg[F, String, String, Boolean] =
        QueryServiceInterp.Eval.make(dataStore, defaultFilter, defaultSort, 5.seconds)

      val result: EitherT[F, QueryFailure, List[String]] = underTest.asList(request)

      result.value
        .unsafeToFuture()
        .futureValue
        .value shouldBe dataValues
    }
  }

  behavior of "Query Service Interpreter asSource"

  it should "fail with an error when paging options are negative" in {
    forAll(Gen.negNum[Int], Gen.negNum[Int]) { (pageNumber, pageSize) =>
      whenever(pageNumber < 0 && pageSize < 0) {
        val filter: Option[String] = None
        val sort: Option[Boolean] = None
        val paging = Some(Paging(pageNumber, pageSize))
        val confirmationDepth = 0

        val request = QueryRequest(filter, sort, paging, confirmationDepth)

        val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
        (dataStore.query _)
          .expects(*, *, *)
          .never()

        val underTest: QueryServiceAlg[F, String, String, Boolean] =
          QueryServiceInterp.Eval.make(dataStore, "", false, 5.seconds)

        val result: EitherT[IO, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

        result.value.unsafeToFuture().futureValue.left.value shouldBe a[QueryFailures.InvalidQuery[String, Boolean]]
      }
    }
  }

  it should "fail with an error when confirmation depth is negative" in {
    forAll(Gen.negNum[Int]) { (confirmationDepth) =>
      whenever(confirmationDepth < 0) {
        val filter: Option[String] = None
        val sort: Option[Boolean] = None
        val paging: Option[Paging] = None

        val request = QueryRequest(filter, sort, paging, confirmationDepth)

        val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
        (dataStore.query _)
          .expects(*, *, *)
          .never()

        val underTest: QueryServiceAlg[F, String, String, Boolean] =
          QueryServiceInterp.Eval.make(dataStore, "", false, 5.seconds)

        val result: EitherT[IO, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

        result.value
          .unsafeToFuture()
          .futureValue
          .left
          .value shouldBe a[QueryFailures.InvalidQuery[String, Boolean]]
      }
    }
  }

  it should "use default filter when no filter provided" in {
    forAll(Gen.asciiStr) { defaultFilter =>
      val filter: Option[String] = None
      val sort: Option[Boolean] = None
      val paging: Option[Paging] = None
      val confirmationDepth = 0

      val dataValues = List.empty[String]

      val defaultSort = true

      val request = QueryRequest(filter, sort, paging, confirmationDepth)

      val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
      (dataStore.query _)
        .expects(defaultFilter, defaultSort, *)
        .once()
        .returns(IO.pure(Source(dataValues)))

      val underTest: QueryServiceAlg[F, String, String, Boolean] =
        QueryServiceInterp.Eval.make(dataStore, defaultFilter, defaultSort, 5.seconds)

      val result: EitherT[F, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

      result.value
        .unsafeToFuture()
        .futureValue
        .value
        .runWith(Sink.seq)
        .futureValue shouldBe dataValues
    }
  }

  it should "use provided filter to query data store" in {
    forAll(Gen.asciiStr) { filterValue =>
      val filter: Option[String] = Some(filterValue)
      val sort: Option[Boolean] = None
      val paging: Option[Paging] = None
      val confirmationDepth = 0

      val dataValues = List.empty[String]

      val defaultFilter = "test"
      val defaultSort = true

      val request = QueryRequest(filter, sort, paging, confirmationDepth)

      val dataStore = mock[DataStoreQueryAlg[F, Source[*, NotUsed], Boolean, String, String]]
      (dataStore.query _)
        .expects(filterValue, defaultSort, *)
        .once()
        .returns(IO.pure(Source(dataValues)))

      val underTest: QueryServiceAlg[F, String, String, Boolean] =
        QueryServiceInterp.Eval.make(dataStore, defaultFilter, defaultSort, 5.seconds)

      val result: EitherT[F, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

      result.value
        .unsafeToFuture()
        .futureValue
        .value
        .runWith(Sink.seq)
        .futureValue shouldBe dataValues
    }
  }
}
