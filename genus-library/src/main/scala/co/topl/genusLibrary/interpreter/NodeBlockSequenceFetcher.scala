package co.topl.genusLibrary.interpreter

import cats.effect.Resource
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.{BlockFetcherAlgebra, BlockSequenceFetcherAlgebra}
import co.topl.genusLibrary.model.{BlockData, HeightData}
import fs2.Stream
import org.typelevel.log4cats.Logger

object NodeBlockSequenceFetcher {

  def make[F[_]: Async: Logger](
    blockFetcher: BlockFetcherAlgebra[F]
  ): Resource[F, BlockSequenceFetcherAlgebra[F, Stream[F, *]]] =
    Resource.pure {

      new BlockSequenceFetcherAlgebra[F, Stream[F, *]] {
        override def fetch(startHeight: Long, endHeight: Long): F[Stream[F, BlockData]] =
          Async[F].delay {
            Stream
              // Range from given start height to either defined max height or "positive infinity".
              // If start height is one, then the range would be [1, 2, 3, ...]
              .range(startHeight, endHeight)
              .covary[F]
              .evalMap(blockFetcher.fetch)
              .evalMapFilter {
                case Left(ex) =>
                  Logger[F]
                    .error(s"Unexpected error while fetching block. Error=[$ex]")
                    .as(Option.empty[BlockData])
                case Right(HeightData(height, None)) =>
                  Logger[F]
                    .info(s"No block found. Height=[$height]")
                    .as(Option.empty[BlockData])
                case Right(HeightData(_, blockData)) =>
                  blockData.pure[F]
              }
          }
      }
    }
}
