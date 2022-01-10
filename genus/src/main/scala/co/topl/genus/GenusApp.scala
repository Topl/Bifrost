package co.topl.genus

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import cats.effect.{Async, IO, IOApp}
import co.topl.genus.algebras.{DatabaseClientAlg, HttpServer, QueryAlg}
import co.topl.genus.interpreters.{MongoDatabaseClient, MongoQuery, MongoSubscription, QueryServer}
import co.topl.genus.programs.GenusProgram
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import com.typesafe.config.{Config, ConfigFactory}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase}

import scala.concurrent.duration.DurationInt

object GenusApp extends IOApp.Simple {

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

  val transactionsQuery: QueryAlg[IO, Source[*, NotUsed], Bson, ConfirmedTransactionDataModel] =
    MongoQuery.Eval.make(txsMongoCollection)

  val blocksQuery: QueryAlg[IO, Source[*, NotUsed], Bson, BlockDataModel] =
    MongoQuery.Eval.make(blocksMongoCollection)

  val databaseClient: DatabaseClientAlg[IO, Source[*, NotUsed]] =
    MongoDatabaseClient.Eval.make(
      transactionsQuery,
      blocksQuery,
      MongoSubscription.Mock.make,
      MongoSubscription.Mock.make
    )

  val server: HttpServer[IO] = QueryServer.Eval.make[IO](databaseClient, 5.seconds)(serverIp, serverPort)

  override def run: IO[Unit] =
    GenusProgram.Mock
      .make[IO](server)
      .flatMap(_ => IO.never)
      .guarantee(Async[IO].fromFuture(IO.delay(system.terminate())).void)
}
