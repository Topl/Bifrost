package co.topl.transactiongenerator

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.implicits._
import cats.effect._
import cats.data._
import co.topl.grpc.ToplGrpc
import co.topl.transactiongenerator.interpreters._
import co.topl.common.application._
import co.topl.interpreters._
import com.typesafe.config.ConfigFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.typeclasses.implicits._
import fs2._
import scala.concurrent.duration._

object TransactionGeneratorApp
    extends IOAkkaApp[Unit, Unit, Nothing](
      _ => (),
      _ => ConfigFactory.load(),
      (_, _) => (),
      (_, _, config) => ActorSystem[Nothing](Behaviors.empty, "TransactionGeneratorApp", config)
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def run: IO[Unit] =
    for {
      // Initialize gRPC Clients
      clients <- NonEmptyChain(("localhost", 9084)).traverse { case (host, port) =>
        ToplGrpc.Client.make[F](host, port, tls = false)
      }
      // Turn the list of clients into a single client (round-robin)
      client <- MultiToplRpc.make(clients)
      // Assemble a base wallet of available UTxOs
      wallet <- ToplRpcWalletInitializer.make[F](client).flatMap(_.initialize)
      // Split the base wallet into multiple wallets to enable parallel participants
      wallets = WalletSplitter.split(wallet, Runtime.getRuntime.availableProcessors())
      // Combine all wallets into a stream of transactions
      transactionStream =
        Stream
          .iterable(wallets)
          .evalMap(Fs2TransactionGenerator.make[F])
          .evalMap(_.generateTransactions)
          .parJoinUnbounded
      // Broadcast the transactions
      _ <- transactionStream
        .groupWithin()
        // Send 1 transaction per _this_ duration
        .metered(500.milli)
        // Broadcast+log the transaction
        .evalTap(transaction =>
          Logger[F].info(show"Broadcasting transaction id=${transaction.id.asTypedBytes}") >>
          client.broadcastTransaction(transaction) >>
          Logger[F].info(show"Broadcasted transaction id=${transaction.id.asTypedBytes}")
        )
        .onError { case e =>
          Stream.eval(Logger[F].error(e)("Stream failed"))
        }
        .compile
        .drain
    } yield ()

}
