package co.topl.genus.interpreters.queryservices

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query.{BlocksQuery, QueryBlocksReq}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class BlocksQueryServiceSpec extends AsyncFlatSpec with AsyncMockFactory with Matchers {
  behavior of "BlocksQueryService.Eval.make query"

  val actorSystem: ActorSystem = ActorSystem("test")
  implicit val materializer: Materializer = Materializer(actorSystem)

  it should "return an empty list of blocks when the database is empty" in {
    val queryFilter = BlockFilter(BlockFilter.FilterType.All(BlockFilter.AllFilter()))
    val queryBlocksReq: QueryBlocksReq = QueryBlocksReq(queryFilter.some)

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryBlocks _)
      .expects(*)
      .returns(Source.empty.pure[IO])

    val underTest: BlocksQuery =
      BlocksQueryService.Eval.make(databaseClient, 5.seconds)

    val result = underTest.query(queryBlocksReq)

    result map { r => r.result.success.get.blocks should be(empty) }
  }

  it should "return all existing transactions when the database is not empty and no filters are provided" in {
    val queryBlocksReq: QueryBlocksReq = QueryBlocksReq(None)
    val existingBlocks: Seq[Block] = Seq(
      Block(id = "test1"),
      Block(id = "test2"),
      Block(id = "test3")
    )

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryBlocks _)
      .expects(*)
      .returns(Source(existingBlocks).pure[IO])

    val underTest: BlocksQuery =
      BlocksQueryService.Eval.make(databaseClient, 5.seconds)

    val result = underTest.query(queryBlocksReq)

    result map { r => r.result.success.get.blocks shouldNot be(empty) }
  }

  it should "return timeout failure when the query takes longer than the configured time to evaluate" in {
    val queryBlocksReq: QueryBlocksReq = QueryBlocksReq(None)

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] = mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryBlocks _)
      .expects(*)
      .returns(Source.never.pure[IO])

    val underTest: BlocksQuery =
      BlocksQueryService.Eval.make(databaseClient, 1.seconds)

    val result = underTest.query(queryBlocksReq)

    result map { r =>
      r.result.failure.get.reason.isQueryTimeout shouldBe true
    }
  }

  it should "return data store error failure when the database client has an error" in {
    val queryBlocksReq: QueryBlocksReq = QueryBlocksReq(None)
    val errorMessage = "error occurred with connection to database!"

    val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] =
      mock[DatabaseClientAlg[IO, Source[*, NotUsed]]]
    (databaseClient.queryBlocks _)
      .expects(*)
      .returns(IO.fromEither(Left(new Throwable(errorMessage))))

    val underTest: BlocksQuery =
      BlocksQueryService.Eval.make(databaseClient, 1.seconds)

    val result = underTest.query(queryBlocksReq)

    result map { r =>
      r.result.failure.get.reason.dataStoreConnectionError.get shouldBe errorMessage
    }
  }
}
