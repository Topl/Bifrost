package co.topl.genus.interpreters.services

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.algebras.QueryService.QueryRequest
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.interpreters.{MockChainHeight, MockMongoStore}
import co.topl.genus.ops.EitherTSourceOps.implicits._
import co.topl.genus.services.transactions_query.TransactionSorting
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Transaction
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.models.{BlockSummaryDataModel, ConfirmedTransactionDataModel}
import org.mongodb.scala.Document
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ListMap

class TransactionsQueryServiceSpec
    extends ScalaTestWithActorTestKit
    with AsyncFlatSpecLike
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory {
  import TransactionsQueryServiceSpec._

  behavior of "Transactions Query Service"

  it should "return all transactions that mongo provides" in {
    val transactions = List(testTransactionModel, testTransactionModel)

    val dataStore = MockMongoStore.withTransactions[IO](transactions)
    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.default[IO]

    val underTest = TransactionsQueryService.make[IO](dataStore)

    val result = underTest.query(defaultQueryRequest).materializeToList

    result.value
      .map(values => values shouldBe Right(transactions.map(_.transformTo[Transaction])))
      .unsafeToFuture()
  }

  it should "not return documents that fail to convert into transactions" in {
    val transaction = testTransactionModel

    val validTransactionDocuments = List(transaction.asDocument)
    val invalidTransactionDocuments = List(Document("{ \"invalid\": \"test\" }"))

    val dataStore = MockMongoStore.withDocuments[IO](validTransactionDocuments ++ invalidTransactionDocuments)
    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.default[IO]

    val underTest = TransactionsQueryService.make[IO](dataStore)

    val result =
      underTest
        .query(defaultQueryRequest)
        .materializeToList

    result.value
      .map(values => values shouldBe Right(List(transaction.transformTo[Transaction])))
      .unsafeToFuture()
  }
}

object TransactionsQueryServiceSpec {

  val testTransactionModel: ConfirmedTransactionDataModel =
    ConfirmedTransactionDataModel(
      BlockSummaryDataModel("test-id", 10),
      "PolyTransfer",
      "100000",
      ListMap.empty,
      Nil,
      None,
      List.empty,
      minting = false,
      "test-tx-id",
      List.empty,
      "1999",
      List.empty,
      "PublicKeyCurve25519"
    )

  val defaultQueryRequest: QueryRequest[TransactionFilter, TransactionSorting] =
    QueryRequest(TransactionFilter.defaultInstance, TransactionSorting.defaultInstance, None, 0)
}
