package co.topl.genus

import akka.actor
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import co.topl.genus.algebras._
import co.topl.genus.interpreters.mongo.{MongoOplogImpl, MongoQueryImpl, MongoSubscriptionImpl}
import co.topl.genus.interpreters.services.{QueryServiceImpl, SubscriptionServiceImpl}
import co.topl.genus.programs.GenusProgram
import co.topl.genus.settings.StartupOpts
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types._
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import com.typesafe.config.ConfigFactory
import mainargs.ParserForClass
import org.mongodb.scala.MongoClient

import scala.concurrent.duration.DurationInt

object GenusApp extends IOApp {

  def make(options: StartupOpts): IO[ExitCode] =
    Resource
      .make(
        IO.delay(
          ActorSystem[Unit](
            Behaviors.empty,
            "genus-guardian",
            ConfigFactory
              .parseString("akka.http.server.preview.enable-http2 = on")
              .withFallback(ConfigFactory.defaultApplication())
          )
        )
      )(system => IO.delay(system.terminate()).flatMap(_ => IO.fromFuture(IO.delay(system.whenTerminated)).void))
      .use { system =>
        implicit val classicSystem: actor.ActorSystem = system.classicSystem
        makeWithSystem(options)
      }

  def makeWithSystem(options: StartupOpts)(implicit system: actor.ActorSystem): IO[ExitCode] =
    for {
      mongoClient <- IO.delay(MongoClient(options.mongoConnectionString))
      mongoDatabase = mongoClient.getDatabase(options.mongoDatabaseName)
      transactionsCollection = mongoDatabase.getCollection(options.transactionsCollectionName)
      blocksCollection = mongoDatabase.getCollection(options.blocksCollectionName)
      oplogCollection = mongoClient.getDatabase(options.localDatabaseName).getCollection(options.oplogCollectionName)
      oplog = MongoOplogImpl.make[IO](oplogCollection)
      transactionsQuery =
        MongoQuery.map[IO, ConfirmedTransactionDataModel, Transaction](
          MongoQueryImpl.make[IO, ConfirmedTransactionDataModel](transactionsCollection),
          _.transformTo[Transaction]
        )
      blocksQuery =
        MongoQuery.map[IO, BlockDataModel, Block](
          MongoQueryImpl.make[IO, BlockDataModel](blocksCollection),
          _.transformTo[Block]
        )
      transactionsSubscription =
        MongoSubscription.map[IO, ConfirmedTransactionDataModel, Transaction](
          MongoSubscriptionImpl.make(
            mongoClient,
            options.mongoDatabaseName,
            options.transactionsCollectionName,
            "block.height",
            oplog
          ),
          _.transformTo[Transaction]
        )
      blocksSubscription =
        MongoSubscription.map[IO, BlockDataModel, Block](
          MongoSubscriptionImpl.make(
            mongoClient,
            options.mongoDatabaseName,
            options.blocksCollectionName,
            "height",
            oplog
          ),
          _.transformTo[Block]
        )
      _ <-
        GenusProgram.make[IO](
          QueryServiceImpl.make(transactionsQuery, options.queryTimeout.milliseconds),
          SubscriptionServiceImpl.make(transactionsSubscription),
          QueryServiceImpl.make(blocksQuery, options.queryTimeout.milliseconds),
          SubscriptionServiceImpl.make(blocksSubscription),
          options.ip,
          options.port
        )
    } yield ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] =
    ParserForClass[StartupOpts]
      .constructEither(args)
      .toEitherT[IO]
      .map(make)
      .value
      .flatMap {
        case Left(error) =>
          IO.println(s"Failed to start application: $error").map[ExitCode](_ => ExitCode.Error)
        case Right(success) => success
      }
}
