package co.topl.transactiongenerator

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.implicits._
import cats.effect._
import cats.data._
import co.topl.grpc.ToplGrpc
import co.topl.transactiongenerator.interpreters._
import co.topl.common.application._
import com.typesafe.config.ConfigFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.typeclasses.implicits._

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
      clients <- NonEmptyChain(("localhost", 9084)).traverse { case (host, port) =>
        ToplGrpc.Client.make[F](host, port, tls = false)
      }
      broadcasters <- clients.traverse(ToplRpcTransactionBroadcaster.make[F])
      broadcaster  <- MultiTransactionBroadcaster.make(broadcasters)
      bigBangId    <- OptionT(clients.head.blockIdAtHeight(1)).getOrRaise(new IllegalStateException())
      bigBangBody  <- OptionT(clients.head.fetchBlockBody(bigBangId)).getOrRaise(new IllegalStateException())
      bigBangTransaction <- OptionT(clients.head.fetchTransaction(bigBangBody.head))
        .getOrRaise(new IllegalStateException())
      generator <- Fs2TransactionGenerator.make[F](bigBangTransaction)
      stream    <- generator.generateTransactions
      _ <- stream
        .evalTap(broadcaster.broadcastTransaction)
        .evalTap(transaction => Logger[F].info(show"Broadcasting transaction id=${transaction.id.asTypedBytes}"))
        .compile
        .drain
    } yield ()

}
