package co.topl.genus

import akka.actor.ActorSystem
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, IOApp}
import co.topl.genus.algebras.{DataStoreQueryAlg, HttpServerAlg, QueryServiceAlg}
import co.topl.genus.filters.{BlockFilter, TransactionFilter}
import co.topl.genus.interpreters.MongoQueryInterp.MongoQueryAlg
import co.topl.genus.interpreters.{MongoQueryInterp, QueryServer, QueryServiceInterp}
import co.topl.genus.programs.{BlocksQueryProgram, RunServerProgram, TransactionsQueryProgram}
import co.topl.genus.services.blocks_query.BlocksQuery
import co.topl.genus.services.transactions_query.TransactionsQuery
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types._
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import com.typesafe.config.{Config, ConfigFactory}
import org.bson.conversions.Bson
import org.mongodb.scala.model.Sorts
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase}

import scala.concurrent.duration.DurationInt

object GenusApp extends IOApp.Simple {

  type F[T] = IO[T]

  val config: Config =
    ConfigFactory
      .parseString("akka.http.server.preview.enable-http2 = on")
      .withFallback(ConfigFactory.defaultApplication())

  implicit val system: ActorSystem = ActorSystem("genus", config)

  val serverIp = "127.0.0.1"
  val serverPort = 8080

  val mongoConnectionString = "mongodb://localhost:27017/?replSet=Bifrost"
  val mongoDatabaseName = "chain_data"
  val txsMongoCollectionName = "confirmed_txes"
  val blocksMongoCollectionName = "blocks"

  val mongoClient: MongoClient = MongoClient(mongoConnectionString)

  val mongoDb: MongoDatabase = mongoClient.getDatabase(mongoDatabaseName)

  val txsMongoCollection: MongoCollection[Document] = mongoDb.getCollection(txsMongoCollectionName)

  val blocksMongoCollection: MongoCollection[Document] = mongoDb.getCollection(blocksMongoCollectionName)

  val txsDataStoreQuery: MongoQueryAlg[F, Transaction, TransactionFilter] =
    DataStoreQueryAlg.mapQueryType(
      MongoQueryInterp.Eval.make[F, ConfirmedTransactionDataModel, TransactionFilter](txsMongoCollection),
      (dataModel: ConfirmedTransactionDataModel) => dataModel.transformTo[Transaction]
    )

  val blocksDataStoreQuery: MongoQueryAlg[F, Block, BlockFilter] =
    DataStoreQueryAlg.mapQueryType(
      MongoQueryInterp.Eval.make[F, BlockDataModel, BlockFilter](blocksMongoCollection),
      (dataModel: BlockDataModel) => dataModel.transformTo[Block]
    )

  val defaultTransactionFilter: TransactionFilter =
    TransactionFilter.of(TransactionFilter.FilterType.All(TransactionFilter.AllFilter()))
  val defaultTransactionSort: Bson = Sorts.ascending("block.height")

  val transactionsQueryService: QueryServiceAlg[F, Transaction, TransactionFilter, Bson] =
    QueryServiceInterp.Eval.make[F, Transaction, TransactionFilter, Bson](
      txsDataStoreQuery,
      defaultTransactionFilter,
      defaultTransactionSort,
      5.seconds
    )

  val defaultBlockFilter: BlockFilter = BlockFilter.of(BlockFilter.FilterType.All(BlockFilter.AllFilter()))
  val defaultBlockSort: Bson = Sorts.ascending("height")

  val blocksQueryService: QueryServiceAlg[F, Block, BlockFilter, Bson] =
    QueryServiceInterp.Eval.make[F, Block, BlockFilter, Bson](
      blocksDataStoreQuery,
      defaultBlockFilter,
      defaultBlockSort,
      5.seconds
    )

  val transactionsQuery: TransactionsQuery = TransactionsQueryProgram.Eval.make[F](
    transactionsQueryService
  )

  val blocksQuery: BlocksQuery = BlocksQueryProgram.Eval.make[F](
    blocksQueryService
  )

  val server: HttpServerAlg[F] =
    QueryServer.Eval.make[IO](transactionsQuery, blocksQuery)(serverIp, serverPort)

  override def run: IO[Unit] =
    RunServerProgram.Eval
      .make[F](server)
      .flatMap(_ => IO.never)
      .guarantee(Async[IO].fromFuture(IO.delay(system.terminate())).void)
}
