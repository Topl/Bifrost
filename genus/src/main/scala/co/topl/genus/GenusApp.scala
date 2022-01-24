package co.topl.genus

import cats.implicits._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, IOApp}
import co.topl.genus.algebras.{HttpServerAlg, QueryServiceAlg}
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
import org.mongodb.scala.bson.conversions.Bson
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

  val mongoClient: MongoClient = MongoClient(mongoConnectionString)

  val mongoDb: MongoDatabase = mongoClient.getDatabase("chain_data")

  val txsMongoCollection: MongoCollection[Document] = mongoDb.getCollection("confirmed_txes")

  val blocksMongoCollection: MongoCollection[Document] = mongoDb.getCollection("blocks")

  val txsDataStoreQuery: MongoQueryAlg[F, Transaction, TransactionFilter] =
    // TODO: figure out how to get this implicit functor to work
    dataStoreQueryAlgFunctor[F, Source[*, NotUsed], Bson, TransactionFilter, Transaction].map(
      MongoQueryInterp.Eval.make[F, ConfirmedTransactionDataModel, TransactionFilter](txsMongoCollection)
    )(_.transformTo[Transaction])

  val blocksDataStoreQuery: MongoQueryAlg[F, Block, BlockFilter] =
    dataStoreQueryAlgFunctor[F, Source[*, NotUsed], Bson, BlockFilter, Block].map(
      MongoQueryInterp.Eval.make[F, BlockDataModel, BlockFilter](blocksMongoCollection)
    )(_.transformTo[Block])

  val transactionsQueryService: QueryServiceAlg[F, Transaction, TransactionFilter, Bson] =
    QueryServiceInterp.Eval.make[F, Transaction, TransactionFilter, Bson](
      txsDataStoreQuery,
      interpreters.defaultTransactionFilter,
      interpreters.defaultTransactionSort,
      5.seconds
    )

  val blocksQueryService: QueryServiceAlg[F, Block, BlockFilter, Bson] =
    QueryServiceInterp.Eval.make[F, Block, BlockFilter, Bson](
      blocksDataStoreQuery,
      interpreters.defaultBlockFilter,
      interpreters.defaultBlockSort,
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
