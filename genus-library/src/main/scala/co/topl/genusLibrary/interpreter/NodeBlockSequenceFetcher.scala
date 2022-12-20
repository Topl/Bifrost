package co.topl.genusLibrary.interpreter

import cats.effect.kernel.Async
import cats.implicits.catsSyntaxOptionId
import cats.implicits._
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, BlockSequenceFetcherAlgebra}
import co.topl.genusLibrary.model.{BlockData, HeightData}
import com.typesafe.scalalogging.LazyLogging
import fs2.Stream

class NodeBlockSequenceFetcher[F[_]: Async](blockFetcher: BlockFetcherAlgebra[F])
    extends BlockSequenceFetcherAlgebra[F, Stream[F, *]]
    with LazyLogging {

  // TODO: TSDK-215 | Implement retry
  override def fetch(startHeight: Long, endHeight: Long = Long.MaxValue): F[Stream[F, BlockData]] = Async[F].delay {
    Stream
      // Range from given start height to either defined max height or "positive infinity".
      // If start height is one, then the range would be [1, 2, 3, ...]
      .range(startHeight, endHeight)
      .covary[F]
      .evalMap(blockFetcher.fetch)
      .mapFilter {
        case Left(ex) =>
          logger error s"Unexpected error while fetching block. Error=[$ex]"
          None
        case Right(HeightData(height, None)) =>
          logger info s"No block found. Height=[$height]"
          None
        case Right(HeightData(_, Some(blockData))) =>
          blockData some
      }
  }

}
