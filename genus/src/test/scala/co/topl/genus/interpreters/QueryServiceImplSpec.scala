package co.topl.genus.interpreters

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.data.{Chain, EitherT}
import cats.effect.{Async, IO}
import cats.effect.testing.scalatest.AsyncIOSpec
import co.topl.genus.algebras.QueryService._
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
import co.topl.genus.interpreters.services.QueryServiceImpl
import co.topl.genus.typeclasses.{MongoFilter, MongoSort}
import org.bson.BsonDocument
import org.bson.conversions.Bson
import org.mongodb.scala.bson.{BsonBoolean, BsonString}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

class QueryServiceImplSpec
    extends AsyncFlatSpec
    with EitherValues
    with AsyncMockFactory
    with Matchers
    with BeforeAndAfterAll
    with AsyncIOSpec {

  implicit val system: ActorSystem = ActorSystem("QueryServiceInterpSpec")

  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  behavior of "Query Service Interpreter asList"

  it should "fail with an error when paging options are negative" in {
    import QueryServiceImplSpec.implicits._

    val pageNumber = -5;
    val pageSize = -10;

    val filter: String = "test-filter"
    val sort: Boolean = false
    val paging = Some(Paging(pageNumber, pageSize))
    val confirmationDepth = 0

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = MockMongoQuery.make[IO, String](Chain.empty)

    val underTest: QueryService[IO, String] = QueryServiceImpl.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, List[String]] = underTest.asList(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }

  it should "fail with an error when confirmation depth is negative" in {
    import QueryServiceImplSpec.implicits._

    val confirmationDepth = -10;
    val filter: String = "test-filter"
    val sort: Boolean = false
    val paging: Option[Paging] = None

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = MockMongoQuery.make[IO, String](Chain.empty)

    val underTest: QueryService[IO, String] = QueryServiceImpl.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, List[String]] = underTest.asList(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }

  behavior of "Query Service Interpreter asSource"

  it should "fail with an error when paging options are negative" in {
    import QueryServiceImplSpec.implicits._

    val pageNumber = -9;
    val pageSize = -100;
    val filter: String = "test-filter"
    val sort: Boolean = true
    val paging = Some(Paging(pageNumber, pageSize))
    val confirmationDepth = 0

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = MockMongoQuery.make[IO, String](Chain.empty)

    val underTest: QueryService[IO, String] = QueryServiceImpl.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }

  it should "fail with an error when confirmation depth is negative" in {
    import QueryServiceImplSpec.implicits._

    val confirmationDepth = -100;
    val filter: String = "test-filter"
    val sort: Boolean = true
    val paging: Option[Paging] = None

    val request = QueryRequest(filter, sort, paging, confirmationDepth)

    val dataStore = MockMongoQuery.make[IO, String](Chain.empty)

    val underTest: QueryService[IO, String] = QueryServiceImpl.make(dataStore, 5.seconds)

    val result: EitherT[IO, QueryFailure, Source[String, NotUsed]] = underTest.asSource(request)

    result.value.asserting(_.left.value shouldBe a[QueryFailures.InvalidQuery])
  }
}

object QueryServiceImplSpec {

  trait Implicits {
    // mock filters for string values
    implicit val stringFilter: MongoFilter[String] = value => new BsonDocument("value", new BsonString(value))
    implicit val stringSort: MongoSort[Boolean] = value => new BsonDocument("value", new BsonBoolean(value))
  }

  object implicits extends Implicits
}
