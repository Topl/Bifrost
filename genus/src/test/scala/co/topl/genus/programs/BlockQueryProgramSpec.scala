package co.topl.genus.programs

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.QueryServiceAlg
import co.topl.genus.algebras.QueryServiceAlg.QueryFailures
import co.topl.genus.filters.BlockFilter
import co.topl.genus.services.blocks_query.{BlocksQuery, BlocksQueryStreamReq, QueryBlocksReq, QueryBlocksRes}
import co.topl.genus.types._
import org.mongodb.scala.bson.conversions.Bson
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.flatspec.AsyncFlatSpec
import co.topl.genus.typeclasses.implicits._
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class BlockQueryProgramSpec
    extends AsyncFlatSpec
    with ScalaFutures
    with EitherValues
    with AsyncMockFactory
    with Matchers
    with BeforeAndAfterAll {

  type F[T] = IO[T]

  implicit val system: ActorSystem = ActorSystem("BlockQueryProgramSpec")

  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  behavior of "Block Query Program query"

  it should "return a data connection error when the data store call fails" in {
    val query = QueryBlocksReq()

    val queryService = mock[QueryServiceAlg[F, Block, BlockFilter, Bson]]
    (queryService.asList _)
      .expects(*)
      .returns(EitherT.left(IO.pure(QueryFailures.DataStoreConnectionError("error"))))

    val underTest: BlocksQuery = BlocksQueryProgram.Eval.make(queryService)

    val result: Future[QueryBlocksRes] = underTest.query(query)

    result.futureValue.result.failure.get.reason.isDataStoreConnectionError shouldBe true
  }

  behavior of "Block Query Program queryStream"

  it should "return the existing values in the data store" in {
    val query = BlocksQueryStreamReq()

    val existingData: List[Block] = List(Block(id = "test-1"), Block(id = "test-2"))

    val queryService = mock[QueryServiceAlg[F, Block, BlockFilter, Bson]]
    (queryService.asSource _)
      .expects(*)
      .returns(EitherT.right(IO.pure(Source(existingData))))

    val underTest = BlocksQueryProgram.Eval.make(queryService)

    val result = underTest.queryStream(query)

    result.runWith(Sink.seq).futureValue.map(_.result.block.get.id) shouldBe existingData.map(_.id)
  }
}
