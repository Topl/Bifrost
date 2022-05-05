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
import co.topl.genus.settings.{ApplicationSettings, FileConfiguration, StartupOptions}
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types._
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import com.typesafe.config.ConfigFactory
import mainargs.ParserForClass
import org.mongodb.scala.MongoClient

import java.io.File
import scala.concurrent.duration.DurationInt

object GenusApp extends IOApp {

  /**
   * Creates a Genus application with the given settings.
   * @param settings the settings to use for configuring the app
   * @return the app encapsulated as an [[IO]] with a result of [[ExitCode]]
   */
  def make(settings: ApplicationSettings): IO[ExitCode] =
    // creates an actor system resource which will be terminated when the resource is released
    Resource
      .make(
        IO.delay(
          ActorSystem(
            Behaviors.empty[Unit],
            "genus-guardian",
            ConfigFactory
              .parseString("akka.http.server.preview.enable-http2 = on")
              .withFallback(ConfigFactory.defaultApplication())
          )
        )
      )(system => IO.delay(system.terminate()).flatMap(_ => IO.fromFuture(IO.delay(system.whenTerminated)).void))
      .use { system =>
        implicit val classicSystem: actor.ActorSystem = system.classicSystem
        // use the system to create the Genus application
        makeWithSystem(settings)
      }

  def makeWithSystem(settings: ApplicationSettings)(implicit system: actor.ActorSystem): IO[ExitCode] =
    for {
      // set up MongoDB resources
      mongoClient <- IO.delay(MongoClient(settings.mongoConnectionString))
      mongoDatabase = mongoClient.getDatabase(settings.mongoDatabaseName)
      transactionsCollection = mongoDatabase.getCollection(settings.transactionsCollectionName)
      blocksCollection = mongoDatabase.getCollection(settings.blocksCollectionName)
      oplogCollection = mongoClient.getDatabase(settings.localDatabaseName).getCollection(settings.oplogCollectionName)
      oplog = MongoOplogImpl.make[IO](oplogCollection)

      // set up query services
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

      // set up subscription services
      transactionsSubscription =
        MongoSubscription.map[IO, ConfirmedTransactionDataModel, Transaction](
          MongoSubscriptionImpl.make(
            mongoClient,
            settings.mongoDatabaseName,
            settings.transactionsCollectionName,
            "block.height",
            oplog
          ),
          _.transformTo[Transaction]
        )
      blocksSubscription =
        MongoSubscription.map[IO, BlockDataModel, Block](
          MongoSubscriptionImpl.make(
            mongoClient,
            settings.mongoDatabaseName,
            settings.blocksCollectionName,
            "height",
            oplog
          ),
          _.transformTo[Block]
        )

      // create a GenusProgram from the services and application settings
      _ <-
        GenusProgram.make[IO](
          QueryServiceImpl.make(transactionsQuery, settings.queryTimeout.milliseconds),
          SubscriptionServiceImpl.make(transactionsSubscription),
          QueryServiceImpl.make(blocksQuery, settings.queryTimeout.milliseconds),
          SubscriptionServiceImpl.make(blocksSubscription),
          settings.ip,
          settings.port
        )
    } yield ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] =
    ParserForClass[StartupOptions]
      .constructEither(args)
      .toEitherT[IO]
      // read from config file and override with command line options
      .flatMap(opts =>
        FileConfiguration
          .make[IO](new File(opts.configurationPath.getOrElse("genus.conf")))
          .read
          .map(settings => ApplicationSettings.mergeWithStartup(settings, opts))
          .leftMap(_.toString)
      )
      // make application
      .map(make)
      .value
      .flatMap {
        case Left(error) =>
          IO.println(s"Failed to start application: $error").map[ExitCode](_ => ExitCode.Error)
        case Right(success) => success
      }
}
