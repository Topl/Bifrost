package co.topl.genusLibrary.interpreter

import cats.data.{EitherT, OptionT}
import cats.effect._
import cats.effect.implicits._
import EitherT._
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import co.topl.genus.services.BlockData
import co.topl.genusLibrary.algebras.NodeBlockFetcherAlgebra
import co.topl.genusLibrary.model.{GE, GEs}
import co.topl.node.models.FullBlockBody
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.collection.immutable.ListSet

object NodeBlockFetcher {

  def make[F[_]: Async: Logger](
    toplRpc:          NodeRpc[F, Stream[F, *]],
    fetchConcurrency: Int
  ): Resource[F, NodeBlockFetcherAlgebra[F, Stream[F, *]]] =
    Resource.pure {

      new NodeBlockFetcherAlgebra[F, Stream[F, *]] {
        override def fetch(startHeight: Long, endHeight: Long): F[Stream[F, BlockData]] =
          Async[F].delay {
            Stream
              // Range from given start height to either defined max height or "positive infinity".
              // If start height is one, then the range would be [1, 2, 3, ...]
              .range(startHeight, endHeight)
              .covary[F]
              .parEvalMap(fetchConcurrency)(fetch)
              .takeWhile(_.exists(_.nonEmpty), takeFailure = true)
              .evalMapFilter {
                case Left(ex) =>
                  Logger[F]
                    .error(s"Unexpected error while fetching block. Error=[$ex]")
                    .as(Option.empty[BlockData])
                case Right(None) =>
                  Logger[F]
                    .info(s"No block found.")
                    .as(Option.empty[BlockData])
                case Right(blockData @ Some(_)) =>
                  (blockData: Option[BlockData]).pure[F]
              }
          }

        override def fetch(height: Long): F[Either[GE, Option[BlockData]]] =
          toplRpc
            .blockIdAtHeight(height)
            .flatMap {
              case Some(blockId) =>
                EitherT(fetch(blockId)).map(_.some).value
              case None =>
                Option.empty[BlockData].asRight[GE].pure[F]
            }

        override def fetch(blockId: BlockId): F[Either[GE, BlockData]] =
          (
            OptionT(toplRpc.fetchBlockHeader(blockId)).toRight(GEs.HeaderNotFound(blockId): GE),
            OptionT(toplRpc.fetchBlockBody(blockId))
              .toRight(GEs.BodyNotFound(blockId): GE)
              .flatMap(body =>
                (
                  fs2.Stream
                    .iterable[EitherT[F, GE, *], TransactionId](body.transactionIds)
                    .parEvalMap(fetchConcurrency)(id =>
                      OptionT(toplRpc.fetchTransaction(id))
                        .toRight(GEs.TransactionsNotFound(ListSet(id)): GE)
                    )
                    .compile
                    .toList,
                  body.rewardTransactionId.traverse(id =>
                    OptionT(toplRpc.fetchTransaction(id))
                      .toRight(GEs.TransactionsNotFound(ListSet(id)): GE)
                  )
                ).parMapN((transactions, reward) => FullBlockBody(transactions, reward))(
                  catsDataParallelForEitherTWithParallelEffect2
                )
              )
          ).parMapN(BlockData(_, _))(catsDataParallelForEitherTWithParallelEffect2).value

        def fetchHeight(): F[Option[Long]] =
          (for {
            headBlockId <- OptionT(toplRpc.blockIdAtDepth(depth = 0))
            blockHeader <- OptionT(toplRpc.fetchBlockHeader(headBlockId))
          } yield blockHeader.height).value

      }
    }
}
