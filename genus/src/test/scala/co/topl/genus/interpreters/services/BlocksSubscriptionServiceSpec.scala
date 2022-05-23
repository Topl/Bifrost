package co.topl.genus.interpreters.services

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.SubscriptionService
import co.topl.genus.algebras.SubscriptionService.CreateSubscriptionFailures
import co.topl.genus.filters.BlockFilter
import co.topl.genus.interpreters.MockMongoSubscription
import co.topl.genus.ops.EitherTSourceOps.implicits._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.models.{BlockDataModel, SimpleValueDataModel, TokenBoxDataModel}
import org.mongodb.scala.Document
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers

class BlocksSubscriptionServiceSpec
    extends ScalaTestWithActorTestKit
    with AsyncFlatSpecLike
    with Matchers
    with ScalaFutures {
  import BlocksSubscriptionServiceSpec._

  behavior of "Blocks Subscription"

  it should "return a connection failure error if the mongo subscription request fails" in {
    val errorMessage = "mongo server not found!"

    val mongoSubscription = MockMongoSubscription.alwaysFailWith(errorMessage)

    val underTest = BlocksSubscriptionService.make(mongoSubscription)

    val result = underTest.create(defaultSubscriptionRequest)

    result.value
      .map(value => value shouldBe Left(CreateSubscriptionFailures.DataConnectionFailure(errorMessage)))
      .unsafeToFuture()
  }

  it should "not return values that fail to deserialize to blocks" in {
    val validBlock = defaultBlock
    val validBlockDocument = defaultBlock.asDocument
    val invalidBlockDocument = Document("{ \"invalid\": true }")

    val mongoSubscription =
      MockMongoSubscription.withDocuments(List(validBlockDocument, invalidBlockDocument))

    val underTest = BlocksSubscriptionService.make[IO](mongoSubscription)

    val result = underTest.create(defaultSubscriptionRequest).materializeToList

    result.value
      .map(value => value shouldBe Right(List(validBlock.transformTo[Block])))
      .unsafeToFuture()
  }
}

object BlocksSubscriptionServiceSpec {

  val defaultBlock: BlockDataModel =
    BlockDataModel(
      "test-block-id",
      "test-block-parent-id",
      "9999",
      TokenBoxDataModel("Polys", "test-box-id", "88888", "test-evidence", SimpleValueDataModel("100000")),
      "test-public-key",
      "test-signature",
      199999,
      "5738462",
      "test-tx-root",
      "test-bloom-filter",
      1,
      1000,
      8889,
      "44"
    )

  val defaultSubscriptionRequest: SubscriptionService.CreateRequest[BlockFilter] =
    SubscriptionService.CreateRequest(BlockFilter.defaultInstance, None, 0)
}
