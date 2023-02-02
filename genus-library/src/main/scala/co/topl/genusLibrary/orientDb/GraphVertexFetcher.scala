package co.topl.genusLibrary.orientDb

import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.orientDb.GenusGraphMetadata._
import co.topl.genusLibrary.orientDb.wrapper.WrappedVertex
import co.topl.genusLibrary.utils.BlockUtils
import co.topl.models.{BlockBody, BlockHeader}
import org.typelevel.log4cats.Logger
import scodec.bits.ByteVector

class GraphVertexFetcher[F[_]: Async: Logger](
  storeFacade: StoreFacade,
  blockUtils:  BlockUtils
) extends VertexFetcher[F] {

  override def fetchHeader(header: BlockHeader): F[Either[Failure, WrappedVertex]] = {
    val blockId = blockUtils.getBlockId(header)
    storeFacade
      .getVertexByField[F](
        blockHeaderSchema.name,
        "blockId",
        blockId
      )
      .map(
        _.toRight(
          Failures.NoCurrentHeaderVertexFailure(ByteVector(blockId)).asInstanceOf[Failure]
        )
      )
      .flatTap {
        case Right(_) =>
          Logger[F].info(s"Found header vertex for block. Height=[${header.height}]")
        case Left(_) =>
          Logger[F].error(s"Didn't find header vertex for block. Height=[${header.height}]")
      }
  }

  override def fetchPreviousHeader(header: BlockHeader): F[Either[Failure, WrappedVertex]] = {
    val blockId = blockUtils.getParentBlockId(header)
    storeFacade
      .getVertexByField[F](
        blockHeaderSchema.name,
        "blockId",
        blockId
      )
      .map(
        _.toRight(
          Failures.NoPreviousHeaderVertexFailure(ByteVector(blockId)).asInstanceOf[Failure]
        )
      )
      .flatTap {
        case Right(_) => Logger[F].info(s"Found previous header vertex for block. Height=[${header.height}]")
        case Left(_) =>
          Logger[F]
            .error(s"Didn't find previous header vertex for block. Height=[${header.height}]")
      }
  }

  override def fetchBody(body: BlockBody, blockHeight: Long): F[Either[Failure, WrappedVertex]] = {
    val transactions = blockUtils.blockBodyToByteArray(body)
    storeFacade
      .getVertexByField[F](
        blockBodySchema.name,
        "transactionIds",
        transactions
      )
      .map(
        _.toRight(
          Failures.NoCurrentBodyVertexFailure(ByteVector(transactions)).asInstanceOf[Failure]
        )
      )
      .flatTap {
        case Right(_) => Logger[F].info(s"Found body vertex for block. Height=[$blockHeight]")
        case Left(_) =>
          Logger[F]
            .error(s"Didn't find body vertex for block. Height=[$blockHeight]")
      }
  }

}
