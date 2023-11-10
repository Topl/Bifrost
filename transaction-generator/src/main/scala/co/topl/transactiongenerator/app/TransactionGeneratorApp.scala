package co.topl.transactiongenerator.app

import cats.Show
import cats.data._
import cats.effect._
import cats.effect.std.Random
import cats.effect.std.SecureRandom
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.syntax._
import co.topl.common.application._
import co.topl.genus.services.TransactionServiceFs2Grpc
import co.topl.grpc.NodeGrpc
import co.topl.interpreters._
import co.topl.transactiongenerator.interpreters._
import co.topl.typeclasses.implicits._
import com.typesafe.config.Config
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object TransactionGeneratorApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => IO.delay(Args.parserArgs.constructOrThrow(args)),
      createConfig = IOBaseApp.createTypesafeConfig(_),
      parseConfig = (_, conf) => IO.delay(ApplicationConfig.unsafe(conf))
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def run(args: Args, config: Config, appConfig: ApplicationConfig): IO[Unit] =
    for {
      _                            <- Logger[F].info(show"Launching Transaction Generator with appConfig=$appConfig")
      implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F]
      // Initialize gRPC Clients
      clientAddresses <- parseClientAddresses(appConfig)
      genusClientAddress = clientAddresses.head
      _ <- Logger[F].info(show"Initializing clients=$clientAddresses")
      genusClientResource = co.topl.grpc
        .makeChannel[F](genusClientAddress._1, genusClientAddress._2, genusClientAddress._3)
        .flatMap(TransactionServiceFs2Grpc.stubResource[F])
      _      <- Logger[F].info(show"Initializing wallet")
      wallet <- genusClientResource.flatMap(GenusWalletInitializer.make[F]).use(_.initialize)
      _      <- Logger[F].info(show"Initialized wallet with spendableBoxCount=${wallet.spendableBoxes.size}")
      // Produce a stream of Transactions from the base wallet
      targetTps = appConfig.transactionGenerator.broadcaster.tps
      _ <- Logger[F].info(show"Generating and broadcasting transactions at tps=$targetTps")
      transactionStream <- Fs2TransactionGenerator
        .make[F](
          wallet,
          appConfig.transactionGenerator.parallelism.generateTx,
          appConfig.transactionGenerator.generator.maxWalletSize
        )
        .flatMap(_.generateTransactions)
      nodeClientsResource = clientAddresses.traverse { case (host, port, useTls) =>
        NodeGrpc.Client.make[F](host, port, tls = useTls)
      }
      // Turn the list of clients into a single client (randomly chosen per-call)
      _ <- nodeClientsResource
        .evalMap(MultiNodeRpc.make[F, NonEmptyChain])
        .use(client =>
          // Broadcast the transactions and run the background mempool stream
          (
            runBroadcastStream(transactionStream, client, targetTps),
            runMempoolStream(client, appConfig.transactionGenerator.mempool.period)
          ).parTupled
        )
    } yield ()

  /**
   * Parse the RPC addresses from configuration.
   * If the address starts with "https://", TLS will be enabled on the connection.
   * If the address starts with "http://", TLS will be disabled on the connection.
   * If the address starts with neither, TLS will be enabled on the connection.
   */
  private def parseClientAddresses(appConfig: ApplicationConfig): F[NonEmptyChain[(String, Int, Boolean)]] =
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
    transactionStream: Stream[F, IoTransaction],
    client:            NodeRpc[F, Stream[F, *]],
    targetTps:         Double
  ) =
    transactionStream
      // Send 1 transaction per _this_ duration
      .metered((1_000_000_000d / targetTps).nanos)
      // Broadcast+log the transaction
      .evalTap(transaction =>
        Logger[F].debug(show"Broadcasting transaction id=${transaction.id}") >>
        client.broadcastTransaction(transaction) >>
        Logger[F].info(show"Broadcasted transaction id=${transaction.id}")
      )
      .onError { case e =>
        Stream.eval(Logger[F].error(e)("Stream failed"))
      }
      .compile
      .drain

  implicit private val showMempool: Show[Set[TransactionId]] =
    catsStdShowForSet(showIoTransactionId)

  /**
   * Periodically poll and log the state of the mempool.
   */
  private def runMempoolStream(client: NodeRpc[F, Stream[F, *]], period: FiniteDuration) =
    Stream
      .awakeEvery[F](period)
      .evalMap(_ => client.currentMempool())
      .evalTap(transactionIds => Logger[F].info(show"Current mempool=$transactionIds"))
      .compile
      .drain

}
