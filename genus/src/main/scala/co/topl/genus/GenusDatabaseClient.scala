package co.topl.genus

import cats.implicits._
import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.Id
import cats.effect.IO
import co.topl.genus.algebras.DatabaseClientAlg
import co.topl.genus.filters.{BlockFilter, TransactionFilter}
import co.topl.genus.interpreters.{MongoClientInterpreter, MongoQuery, MongoSubscription}
import co.topl.genus.types.{Block, Transaction}
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import org.mongodb.scala.MongoClient
import co.topl.utils.mongodb.codecs._

trait GenusDatabaseClient {

  def subscribeToTransactions(
    filter: TransactionFilter
  ): Source[Transaction, NotUsed]

  def subscribeToBlocks(filter: BlockFilter): Source[Block, NotUsed]

  def queryTransactions(filter: TransactionFilter): IO[List[Transaction]]

  def queryBlocks(filter: BlockFilter): IO[List[Block]]
}

object GenusDatabaseClient {

  def apply(
    clientAlg:             DatabaseClientAlg[Id, Source[*, NotUsed]]
  )(implicit materializer: Materializer): GenusDatabaseClient =
    new GenusDatabaseClient {

      override def subscribeToTransactions(filter: TransactionFilter): Source[Transaction, NotUsed] =
        clientAlg.subscribeToTransactions(filter)

      override def subscribeToBlocks(filter: BlockFilter): Source[Block, NotUsed] =
        clientAlg.subscribeToBlocks(filter)

      override def queryTransactions(filter: TransactionFilter): IO[List[Transaction]] =
        IO.fromFuture(clientAlg.queryTransactions(filter).runWith(Sink.seq).pure[IO]).map(_.toList)

      override def queryBlocks(filter: BlockFilter): IO[List[Block]] =
        IO.fromFuture(clientAlg.queryBlocks(filter).runWith(Sink.seq).pure[IO]).map(_.toList)
    }

  def mongoDB(
    mongoClient:                MongoClient,
    databaseName:               String,
    transactionsCollectionName: String = "transactions",
    blocksCollectionName:       String = "blocks"
  )(implicit materializer:      Materializer): GenusDatabaseClient = {
    val mongoDatabase = mongoClient.getDatabase(databaseName)
    val transactionsCollection = mongoDatabase.getCollection(transactionsCollectionName)
    val blocksCollection = mongoDatabase.getCollection(blocksCollectionName)

    val transactionsQuery = MongoQuery.Eval.make[Id, ConfirmedTransactionDataModel](transactionsCollection)
    val transactionsSubscription =
      MongoSubscription.Eval.make[Id, ConfirmedTransactionDataModel](transactionsCollection)

    val blocksQuery = MongoQuery.Eval.make[Id, BlockDataModel](blocksCollection)
    val blocksSubscription = MongoSubscription.Eval.make[Id, BlockDataModel](blocksCollection)

    GenusDatabaseClient(
      MongoClientInterpreter.Eval.make(transactionsQuery, blocksQuery, transactionsSubscription, blocksSubscription)
    )
  }
}
