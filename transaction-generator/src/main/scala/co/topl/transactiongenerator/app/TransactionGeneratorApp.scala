package co.topl.transactiongenerator.app

import cats.Show
import cats.data._
import cats.effect._
import cats.effect.std.Random
import cats.implicits._
import co.topl.algebras.ToplRpc
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.common.application._
import co.topl.grpc.ToplGrpc
import co.topl.interpreters._
import co.topl.models._
import co.topl.transactiongenerator.interpreters._
import co.topl.typeclasses.implicits._
import com.typesafe.config.ConfigFactory
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.collection.immutable.ListSet
import scala.concurrent.duration._

object TransactionGeneratorApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf)
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def run: IO[Unit] =
    for {
      _                            <- Logger[F].info(show"Launching Transaction Generator with appConfig=$appConfig")
      implicit0(random: Random[F]) <- Random.javaSecuritySecureRandom[F]
      // Initialize gRPC Clients
      clientAddresses <- parseClientAddresses
      _               <- Logger[F].info(show"Initializing clients=$clientAddresses")
      clients = clientAddresses.traverse { case (host, port, useTls) =>
        ToplGrpc.Client.make[F](host, port, tls = useTls)
      }
      // Turn the list of clients into a single client (randomly chosen per-call)
      _ <- clients
        .evalMap(MultiToplRpc.make[F, NonEmptyChain])
        .use(client =>
          for {
            // Assemble a base wallet of available UTxOs
            _ <- Logger[F].info(show"Initializing wallet")
            wallet <- ToplRpcWalletInitializer
              .make[F](
                client,
                appConfig.transactionGenerator.parallelism.fetchHeader,
                appConfig.transactionGenerator.parallelism.fetchBody,
                appConfig.transactionGenerator.parallelism.fetchTransaction
              )
              .flatMap(_.initialize)
            _ <- Logger[F].info(show"Initialized wallet with spendableBoxCount=${wallet.spendableBoxes.size}")
            // Produce a stream of Transactions from the base wallet
            targetTps = appConfig.transactionGenerator.broadcaster.tps
            _ <- Logger[F].info(show"Generating and broadcasting transactions at tps=$targetTps")
            transactionStream <- Fs2TransactionGenerator
              .make[F](
                wallet,
                appConfig.transactionGenerator.parallelism.generateTx,
                appConfig.transactionGenerator.generator.maxWalletSize,
                appConfig.transactionGenerator.generator.dataLength
              )
              .flatMap(_.generateTransactions)
            // Broadcast the transactions and run the background mempool stream
            _ <- (
              runBroadcastStream(transactionStream, client, targetTps),
              runMempoolStream(client, appConfig.transactionGenerator.mempool.period)
            ).parTupled
          } yield ()
        )
    } yield ()

  /**
   * Parse the RPC addresses from configuration.
   * If the address starts with "https://", TLS will be enabled on the connection.
   * If the address starts with "http://", TLS will be disabled on the connection.
   * If the address starts with neither, TLS will be enabled on the connection.
   */
  private def parseClientAddresses: F[NonEmptyChain[(String, Int, Boolean)]] =
    IO.fromEither(
      NonEmptyChain
        .fromSeq(appConfig.transactionGenerator.rpc.clients)
        .toRight[Exception](new IllegalArgumentException("No RPC clients specified"))
        .flatMap(_.traverse { string =>
          val (withoutProtocol: String, useTls: Boolean) =
            if (string.startsWith("http://")) (string.drop(7), false)
            else if (string.startsWith("https://")) (string.drop(8), true)
            else (string, true)

          withoutProtocol.split(':').toList match {
            case host :: port :: Nil =>
              port.toIntOption.toRight(new IllegalArgumentException("Invalid RPC port provided")).map((host, _, useTls))
            case _ => Left(new IllegalArgumentException("Invalid RPC config provided"))
          }
        })
    )

  /**
   * Broadcasts each transaction from the input stream
   */
  private def runBroadcastStream(
    transactionStream: Stream[F, Transaction],
    client:            ToplRpc[F, Stream[F, *]],
    targetTps:         Double
  ) =
    transactionStream
      // Send 1 transaction per _this_ duration
      .metered((1_000_000_000d / targetTps).nanos)
      // Broadcast+log the transaction
      .evalTap(transaction =>
        Logger[F].debug(show"Broadcasting transaction id=${transaction.id.asTypedBytes}") >>
        client.broadcastTransaction(transaction) >>
        Logger[F].info(show"Broadcasted transaction id=${transaction.id.asTypedBytes}")
      )
      .onError { case e =>
        Stream.eval(Logger[F].error(e)("Stream failed"))
      }
      .compile
      .drain

  implicit private val showMempool: Show[Set[TypedIdentifier]] =
    catsStdShowForSet(showTypedIdentifier)

  /**
   * Periodically poll and log the state of the mempool.
   */
  private def runMempoolStream(client: ToplRpc[F, Stream[F, *]], period: FiniteDuration) =
    Stream
      .awakeEvery[F](period)
      .evalMap(_ => client.currentMempool())
      .evalTap(transactionIds => Logger[F].info(show"Current mempool=$transactionIds"))
      .compile
      .drain

}
