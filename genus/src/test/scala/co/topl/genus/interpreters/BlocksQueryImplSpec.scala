package co.topl.genus.interpreters

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.QueryService.QueryFailures
import co.topl.genus.interpreters.requesthandlers.BlocksQueryImpl
import co.topl.genus.services.blocks_query._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, EitherValues}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class BlocksQueryImplSpec
    extends AnyFlatSpec
    with ScalaFutures
    with EitherValues
    with MockFactory
    with Matchers
    with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("BlockQueryProgramSpec")

  override def afterAll(): Unit = Await.result(system.terminate(), 5.seconds)

  behavior of "Block Query Program query"

  it should "return a data connection error when the query service call fails" in {
    val query = QueryBlocksReq()

    val queryService = MockQueryService.makeFailing[IO, Block](QueryFailures.DataStoreConnectionError("error"))

    val underTest: BlocksQuery = BlocksQueryImpl.make(queryService)

    val result: Future[QueryBlocksRes] = underTest.query(query)

    result.futureValue.result.failure.get.reason.isDataStoreConnectionError shouldBe true
  }

  behavior of "Block Query Program queryStream"

  it should "return the existing values in the data store" in {
    implicit val patienceConfig: PatienceConfig = PatienceConfig(timeout = Span(1, Seconds))

    val query = BlocksQueryStreamReq()

    val existingData = Chain(Block(id = "test-1"), Block(id = "test-2"))

    val queryService = MockQueryService.makeSuccessful[IO, Block](existingData)

    val underTest = BlocksQueryImpl.make(queryService)

    val result = underTest.queryStream(query)

    result.runWith(Sink.seq).futureValue.map(_.result.block.get.id) shouldBe existingData.toList.map(_.id)
  }
}
