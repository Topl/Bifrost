package co.topl.transactiongenerator

import cats.implicits._
import cats.effect._
import cats.data._
import cats.effect.std.Random
import co.topl.grpc.ToplGrpc
import co.topl.transactiongenerator.interpreters._
import co.topl.common.application._
import co.topl.algebras.ToplRpc
import co.topl.interpreters._
import co.topl.models._
import com.typesafe.config.ConfigFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.typeclasses.implicits._
import fs2._

import scala.concurrent.duration._

object TransactionGeneratorApp
    extends IOBaseApp[Unit, Unit](
      _ => (),
      _ => ConfigFactory.load(),
      (_, _) => ()
    ) {

  val TargetTransactionsPerSecond = TPS.Bitcoin
  val ClientAddresses = NonEmptyChain(("localhost", 9084))

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def run: IO[Unit] =
    for {
      implicit0(random: Random[F]) <- Random.javaSecuritySecureRandom[F]
      // Initialize gRPC Clients
      _ <- Logger[F].info(show"Initializing clients=$ClientAddresses")
      clients = ClientAddresses.traverse { case (host, port) =>
        ToplGrpc.Client.make[F](host, port, tls = false)
      }
      // Turn the list of clients into a single client (randomly chosen per-call)
      _ <- clients
        .evalMap(MultiToplRpc.make[F, NonEmptyChain])
        .use(client =>
          for {
            // Assemble a base wallet of available UTxOs
            _      <- Logger[F].info(show"Initializing wallet")
            wallet <- ToplRpcWalletInitializer.make[F](client).flatMap(_.initialize)
            _      <- Logger[F].info(show"Initialized wallet with spendableBoxCount=${wallet.spendableBoxes.size}")
            // Produce a stream of Transactions from the base wallet
            _ <- Logger[F].info(show"Generating and broadcasting transactions at tps=$TargetTransactionsPerSecond")
            transactionStream <- Fs2TransactionGenerator.make[F](wallet).flatMap(_.generateTransactions)
            // Broadcast the transactions and run the background mempool stream
            _ <- (runBroadcastStream(transactionStream, client), runMempoolStream(client)).parTupled
          } yield ()
        )
    } yield ()

  /**
   * Broadcasts each transaction from the input stream
   */
  private def runBroadcastStream(transactionStream: Stream[F, Transaction], client: ToplRpc[F]) =
    transactionStream
      // Send 1 transaction per _this_ duration
      .metered((1_000_000_000 / TargetTransactionsPerSecond).nanos)
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

  /**
   * Periodically poll and log the state of the mempool.
   */
  private def runMempoolStream(client: ToplRpc[F]) =
    Stream
      .awakeEvery[F](5.seconds)
      .evalMap(_ => client.currentMempool())
      .evalTap(transactionIds => Logger[F].info(show"Current mempool=${transactionIds.toSeq}"))
      .compile
      .drain

}
