package co.topl.genus

import cats.implicits._
import cats.data._
import cats.effect.Async
import cats.effect.implicits._
import cats.effect.kernel.Outcome
import cats.effect.kernel.Resource
import co.topl.algebras.SynchronizationTraversalStep
import co.topl.algebras.SynchronizationTraversalSteps
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.model.GE
import co.topl.typeclasses.implicits.showBlockId
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object Replicator {

  def background[F[_]: Async](genus: Genus[F, fs2.Stream[F, *]]): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    stream(genus).compile.drain.background

  def stream[F[_]: Async](genus: Genus[F, fs2.Stream[F, *]]): fs2.Stream[F, Unit] =
    for {
      implicit0(logger: Logger[F]) <- fs2.Stream.eval(Slf4jLogger.fromName("Genus.Replicator"))
      _                            <- fs2.Stream.sleep[F](20.seconds)
      nodeLatestHeight <- fs2.Stream.eval(
        OptionT(genus.nodeBlockFetcher.fetchHeight()).getOrRaise(new IllegalStateException("Unknown node height"))
      )
      graphCurrentHeight <- fs2.Stream.eval(
        OptionT(
          genus.blockFetcher
            .fetchCanonicalHead()
            .rethrow
        ).fold(0L)(_.height)
      )
      _ <- fs2.Stream.eval(
        Logger[F].info(s"Historical data start=${graphCurrentHeight + 1}, end=${nodeLatestHeight}")
      )
      // Historical + live data streams
      _ <- fs2.Stream
        .force[F, BlockData](
          genus.nodeBlockFetcher.fetch(startHeight = graphCurrentHeight + 1, endHeight = nodeLatestHeight + 3)
        )
        .evalTap(blockData => Logger[F].info(s"Inserting block data ${blockData.header.id.show}"))
        .evalMap(genus.blockUpdater.insert)
        .rethrow ++
      fs2.Stream
        .force[F, SynchronizationTraversalStep](
          genus.nodeRpcClient.synchronizationTraversal()
        )
        .evalMap {
          case SynchronizationTraversalSteps.Applied(blockId) =>
            EitherT(genus.nodeBlockFetcher.fetch(blockId))
              .semiflatTap(blockData => Logger[F].info(s"Inserting block ${blockData.header.id.show}"))
              .flatMapF(genus.blockUpdater.insert)
              .value
          case SynchronizationTraversalSteps.Unapplied(blockId) =>
            EitherT(genus.nodeBlockFetcher.fetch(blockId))
              .semiflatTap(blockData => Logger[F].info(s"Deleting block ${blockData.header.id.show}"))
              .flatMapF(genus.blockUpdater.remove)
              .value
        }
        .rethrow
        .recover(_ => ().asRight[GE])

    } yield ()
}
