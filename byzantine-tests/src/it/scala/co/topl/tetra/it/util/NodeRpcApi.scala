package co.topl.tetra.it.util

import cats.Applicative
import cats.effect._
import cats.implicits._
import co.topl.algebras.{SynchronizationTraversalSteps, ToplRpc}
import co.topl.consensus.models.BlockHeader
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration._

class NodeRpcApi[F[_]](val client: ToplRpc[F, Stream[F, *]]) extends AnyVal {

  def adoptedHeaders: Stream[F, BlockHeader] =
    Stream
      .force(client.synchronizationTraversal())
      .collect { case SynchronizationTraversalSteps.Applied(id) =>
        id
      }
      .evalMap(client.fetchBlockHeader)
      .map(_.get)

  def waitForRpcStartUp(implicit asyncF: Async[F], loggerF: Logger[F]): F[Unit] =
    for {
      _ <- Logger[F].info("Waiting for RPC to start up")
      genesisHeader <-
        Stream
          .retry(
            client
              .blockIdAtHeight(1)
              .map(_.get)
              .flatMap(client.fetchBlockHeader)
              .map(_.get),
            250.milli,
            identity,
            200
          )
          .compile
          .lastOrError
      _        <- Logger[F].info(s"Node RPC is ready. Awaiting Genesis block timestamp=${genesisHeader.timestamp}")
      duration <- Async[F].realTimeInstant.map(genesisHeader.timestamp - _.toEpochMilli).map(_.milli)
      _        <- Applicative[F].whenA(duration.toMillis > 0)(Async[F].sleep(duration))
      _        <- Logger[F].info("Genesis block timestamp reached.  Node is ready.")
    } yield ()
}
