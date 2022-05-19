package co.topl.genus

import akka.actor
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits._
import co.topl.genus.interpreters._
import co.topl.genus.interpreters.requesthandlers._
import co.topl.genus.interpreters.services._
import co.topl.genus.programs.GenusProgram
import co.topl.genus.settings._
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.ops.implicits._
import co.topl.utils.StringDataTypes.Base58Data
import com.typesafe.config.ConfigFactory
import mainargs.ParserForClass
import org.mongodb.scala.{Document, MongoClient}

import java.io.File
import scala.concurrent.ExecutionContext
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

  def makeWithSystem(settings: ApplicationSettings)(implicit system: actor.ActorSystem): IO[ExitCode] = {
    implicit val executionContext: ExecutionContext = system.dispatcher

    for {
      // construct the expected API key
      apiKey <-
        if (settings.disableAuth) none[Base58Data].pure[IO]
        else
          Base58Data
            .validated(settings.apiKeyHash)
            .map(_.some.pure[IO])
            .valueOr(errors => IO.raiseError(new Throwable(s"invalid API key: $errors")))

      // set up MongoDB resources
      mongoClient <- IO.delay(MongoClient(settings.mongoConnectionString))
      mongoDatabase = mongoClient.getDatabase(settings.mongoDatabaseName)

      transactionsStore = MongoCollectionStore.make[IO](
        mongoDatabase.getCollection(settings.transactionsCollectionName)
      )
      blocksStore = MongoCollectionStore.make[IO](mongoDatabase.getCollection(settings.blocksCollectionName))

      chainHeight = MongoChainHeight.make[IO](blocksStore)

      // set up query services
      transactionsQuery = TransactionsQueryService.make[IO](transactionsStore, chainHeight)
      blocksQuery = BlocksQueryService.make[IO](blocksStore, chainHeight)

      // set up subscription services
      transactionsSubscription =
        TransactionsSubscriptionService.make[IO](
          BatchedMongoSubscription.make[IO](
            settings.subBatchSize,
            settings.subBatchSleepDuration.seconds,
            _.getTransactionBlockHeight,
            transactionsStore,
            chainHeight
          )
        )

      blocksSubscription =
        BlocksSubscriptionService.make[IO](
          BatchedMongoSubscription.make[IO](
            settings.subBatchSize,
            settings.subBatchSleepDuration.seconds,
            _.getBlockHeight,
            blocksStore,
            chainHeight
          )
        )

      // create a GenusProgram from the services and application settings
      _ <-
        GenusProgram.make[IO](
          HandleTransactionsQuery.make(transactionsQuery),
          HandleTransactionsSubscription.make(transactionsSubscription),
          HandleBlocksQuery.make(blocksQuery),
          HandleBlocksSubscription.make(blocksSubscription),
          settings.ip,
          settings.port,
          apiKey
        )
    } yield ExitCode.Success
  }

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
