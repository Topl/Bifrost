package co.topl.genus

import akka.NotUsed
import akka.actor.ActorSystem
import akka.grpc.scaladsl.ServerReflection
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.stream.scaladsl.Source
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, IOApp}
import co.topl.genus.algebras._
import co.topl.genus.filters.{BlockFilter, TransactionFilter}
import co.topl.genus.interpreters.MongoQueryInterp.MongoQueryAlg
import co.topl.genus.interpreters.MongoSubscriptionInterp.MongoSubscriptionAlg
import co.topl.genus.interpreters._
import co.topl.genus.programs._
import co.topl.genus.services.blocks_query.{BlockSorting, BlocksQuery, BlocksQueryHandler}
import co.topl.genus.services.blocks_subscription.{BlocksSubscription, BlocksSubscriptionHandler}
import co.topl.genus.services.transactions_query.{TransactionSorting, TransactionsQuery, TransactionsQueryHandler}
import co.topl.genus.services.transactions_subscription.{TransactionsSubscription, TransactionsSubscriptionHandler}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types._
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import com.typesafe.config.{Config, ConfigFactory}
import org.bson.conversions.Bson
import org.mongodb.scala.model.Sorts
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

object GenusApp extends IOApp.Simple {

  type F[T] = IO[T]

  // akka setup
  val config: Config =
    ConfigFactory
      .parseString("akka.http.server.preview.enable-http2 = on")
      .withFallback(ConfigFactory.defaultApplication())

  implicit val system: ActorSystem = ActorSystem("genus", config)

  // server constants
  val serverIp = "127.0.0.1"
  val serverPort = 8080

  // tx and block mongo names
  val mongoConnectionString = "mongodb://localhost:27017/?replicaset=bifrost"
  val databaseName = "chain_data"
  val txsCollectionName = "confirmed_txes"
  val blocksCollectionName = "blocks"

  // oplog mongo names
  val localDatabase = "local"
  val oplogCollectionName = "oplog.rs"

  // mongo clients and database
  val mongoClient: MongoClient = MongoClient(mongoConnectionString)
  val mongoDb: MongoDatabase = mongoClient.getDatabase(databaseName)

  // mongo collections
  val oplogCollection: MongoCollection[Document] =
    mongoClient.getDatabase(localDatabase).getCollection(oplogCollectionName)
  val txsMongoCollection: MongoCollection[Document] = mongoDb.getCollection(txsCollectionName)
  val blocksMongoCollection: MongoCollection[Document] = mongoDb.getCollection(blocksCollectionName)

  // mongo algebras
  val mongoOplog: MongoOplogAlg[F] = MongoOplogInterp.Eval.make(oplogCollection)

  val txsDataStoreQuery: MongoQueryAlg[F, Transaction, TransactionFilter, TransactionSorting] =
    DataStoreQueryAlg
      .mapQueryType[F, Source[
        *,
        NotUsed
      ], TransactionSorting, TransactionFilter, ConfirmedTransactionDataModel, Transaction](
        MongoQueryInterp.Eval.make[F, ConfirmedTransactionDataModel, TransactionFilter, TransactionSorting](
          txsMongoCollection
        ),
        dataModel => dataModel.transformTo[Transaction]
      )

  val blocksDataStoreQuery: MongoQueryAlg[F, Block, BlockFilter, BlockSorting] =
    DataStoreQueryAlg
      .mapQueryType[F, Source[*, NotUsed], BlockSorting, BlockFilter, BlockDataModel, Block](
        MongoQueryInterp.Eval.make[F, BlockDataModel, BlockFilter, BlockSorting](blocksMongoCollection),
        dataModel => dataModel.transformTo[Block]
      )

  val txsDataStoreSub: MongoSubscriptionAlg[F, Transaction, TransactionFilter] =
    DataStoreSubscriptionAlg
      .mapSubscriptionType[F, Source[*, NotUsed], TransactionFilter, ConfirmedTransactionDataModel, Transaction](
        MongoSubscriptionInterp.Eval.make[F, ConfirmedTransactionDataModel, TransactionFilter](
          mongoClient,
          databaseName,
          txsCollectionName,
          "block.height",
          mongoOplog
        ),
        dataModel => dataModel.transformTo[Transaction]
      )

  val blocksDataStoreSub: MongoSubscriptionAlg[F, Block, BlockFilter] =
    DataStoreSubscriptionAlg
      .mapSubscriptionType[F, Source[*, NotUsed], BlockFilter, BlockDataModel, Block](
        MongoSubscriptionInterp.Eval.make[F, BlockDataModel, BlockFilter](
          mongoClient,
          databaseName,
          blocksCollectionName,
          "height",
          mongoOplog
        ),
        dataModel => dataModel.transformTo[Block]
      )

  // default filters and sorts
  val defaultTransactionFilter: TransactionFilter =
    TransactionFilter.of(TransactionFilter.FilterType.All(TransactionFilter.AllFilter()))
  val defaultTransactionSort: Bson = Sorts.ascending("block.height")
  val defaultBlockFilter: BlockFilter = BlockFilter.of(BlockFilter.FilterType.All(BlockFilter.AllFilter()))
  val defaultBlockSort: Bson = Sorts.ascending("height")

  // service implementations
  val transactionsQueryService: QueryServiceAlg[F, Transaction, TransactionFilter, TransactionSorting] =
    QueryServiceInterp.Eval.make(txsDataStoreQuery, 5.seconds)

  val blocksQueryService: QueryServiceAlg[F, Block, BlockFilter, BlockSorting] =
    QueryServiceInterp.Eval.make(blocksDataStoreQuery, 5.seconds)

  val txsSubService: SubscriptionServiceAlg[F, Transaction, TransactionFilter] =
    SubscriptionServiceInterp.Eval.make(defaultTransactionFilter, txsDataStoreSub)

  val blocksSubService: SubscriptionServiceAlg[F, Block, BlockFilter] =
    SubscriptionServiceInterp.Eval.make(defaultBlockFilter, blocksDataStoreSub)

  // HTTP service handlers
  val transactionsQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
    TransactionsQueryHandler.partial(TransactionsQueryProgram.Eval.make(transactionsQueryService))

  val blocksQueryHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
    BlocksQueryHandler.partial(BlocksQueryProgram.Eval.make(blocksQueryService))

  val transactionsSubHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
    TransactionsSubscriptionHandler.partial(TxsSubscriptionProgram.Eval.make(txsSubService))

  val blocksSubHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
    BlocksSubscriptionHandler.partial(BlocksSubscriptionProgram.Eval.make(blocksSubService))

  val reflectionServiceHandler: PartialFunction[HttpRequest, Future[HttpResponse]] =
    ServerReflection.partial(
      List(TransactionsQuery, BlocksQuery, TransactionsSubscription, BlocksSubscription)
    )

  val server: HttpServerAlg[F] =
    HttpServerInterp.Eval.make[IO](
      transactionsQueryHandler,
      blocksQueryHandler,
      transactionsSubHandler,
      blocksSubHandler,
      reflectionServiceHandler
    )(serverIp, serverPort)

  override def run: IO[Unit] =
    RunServerProgram.Eval
      .make[F](server)
      .flatMap(_ => IO.never)
      .guarantee(Async[IO].fromFuture(IO.delay(system.terminate())).void)
}
