package co.topl.genus.interpreters.services

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.SubscriptionService
import co.topl.genus.algebras.SubscriptionService.CreateSubscriptionFailures
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.interpreters.MockMongoSubscription
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers
import co.topl.genus.typeclasses.implicits._
import co.topl.utils.mongodb.models.{BlockSummaryDataModel, ConfirmedTransactionDataModel}
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.codecs._
import org.mongodb.scala.Document
import co.topl.genus.ops.EitherTSourceOps.implicits._
import co.topl.genus.types.Transaction

import scala.collection.immutable.ListMap

class TransactionsSubscriptionServiceSpec
    extends ScalaTestWithActorTestKit
    with AsyncFlatSpecLike
    with Matchers
    with ScalaFutures {

  import TransactionsSubscriptionServiceSpec._

  behavior of "Transactions Subscription Service"

  it should "return a connection failure error if the mongo subscription request fails" in {
    val errorMessage = "unable to connect to mongo!"

    val mongoSubscription = MockMongoSubscription.alwaysFailWith(errorMessage)

    val underTest = TransactionsSubscriptionService.make[IO](mongoSubscription)

    val result = underTest.create(defaultSubscriptionRequest)

    result.value
      .map(value => value shouldBe Left(CreateSubscriptionFailures.DataConnectionFailure(errorMessage)))
      .unsafeToFuture()
  }

  it should "not return values that fail to deserialize to transactions" in {
    val validTransaction = defaultTransaction
    val validTransactionDocument = validTransaction.asDocument
    val invalidTransactionDocument = Document("{ \"invalid\": true }")

    val mongoSubscription =
      MockMongoSubscription.withDocuments(List(invalidTransactionDocument, validTransactionDocument))

    val underTest = TransactionsSubscriptionService.make[IO](mongoSubscription)

    val result = underTest.create(defaultSubscriptionRequest).materializeToList

    result.value
      .map(value => value shouldBe Right(List(validTransaction.transformTo[Transaction])))
      .unsafeToFuture()
  }
}

object TransactionsSubscriptionServiceSpec {

  val defaultSubscriptionRequest: SubscriptionService.CreateRequest[TransactionFilter] =
    SubscriptionService.CreateRequest(TransactionFilter.defaultInstance, None, 0)

  val defaultTransaction: ConfirmedTransactionDataModel =
    ConfirmedTransactionDataModel(
      BlockSummaryDataModel("test-block-id", 100),
      "PolyTransfer",
      "10000",
      ListMap.empty,
      List.empty,
      None,
      List.empty,
      minting = false,
      "test-tx-id",
      List.empty,
      "99",
      List.empty,
      "PublicKeyCurve25519"
    )
}
