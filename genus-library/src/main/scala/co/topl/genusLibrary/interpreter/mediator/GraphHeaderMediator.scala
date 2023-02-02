package co.topl.genusLibrary.interpreter.mediator

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.mediator.HeaderMediatorAlgebra
import co.topl.genusLibrary.failure.{Failure, Failures}
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.GenusGraphMetadata._
import co.topl.genusLibrary.orientDb.StoreFacade
import co.topl.genusLibrary.utils.BlockUtils
import co.topl.models.{BlockBody, BlockHeader}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scodec.bits.ByteVector

/**
 * Implementation of the Header Mediator. After each element insertion, the mediation should take place on the Store.
 * For more information, refer to the HeaderMediatorAlgebra trait.
 * @param storeFacade Facade to interact with the store
 * @param blockUtils Utils to retrieve different values from a block
 * @tparam F the effect-ful context to retrieve the value in
 */
class GraphHeaderMediator[F[_]: Async](
  storeFacade: StoreFacade,
  blockUtils:  BlockUtils
) extends HeaderMediatorAlgebra[F] {

  implicit private val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  override def mediate(block: BlockData): F[Either[Failure, Unit]] = {
    val graph = storeFacade.getGraph
    graph.withEffectfulTransaction {
      (
        for {
          currentHeaderVertex <- EitherT(fetchCurrentHeaderVertex(block.header).flatTap {
            case Right(_) =>
              Logger[F].info(s"Found header vertex for block. Height=[${block.header.height}]")
            case Left(_) =>
              Logger[F].error(s"Didn't find header vertex for block. Height=[${block.header.height}]")
          })
          previousHeaderVertex <- EitherT(fetchPreviousHeaderVertex(block.header).flatTap {
            case Right(_) => Logger[F].info(s"Found previous header vertex for block. Height=[${block.header.height}]")
            case Left(_) =>
              Logger[F]
                .error(s"Didn't find previous header vertex for block. Height=[${block.header.height}]")
          })
          bodyVertex <- EitherT(fetchBodyVertex(block.body).flatTap {
            case Right(_) => Logger[F].info(s"Found body vertex for block. Height=[${block.header.height}]")
            case Left(_) =>
              Logger[F]
                .error(s"Didn't find body vertex for block. Height=[${block.header.height}]")
          })
          _ <- EitherT(
            graph.addEdge(previousHeaderVertex, currentHeaderVertex).asRight[Failure].pure[F]
          )
          headerAndBodyEdge <- EitherT(
            graph.addEdge(currentHeaderVertex, bodyVertex).asRight[Failure].pure[F]
          )
        } yield headerAndBodyEdge
      ).value.map(_.map(_ => ()))
    }
  }

  private def fetchCurrentHeaderVertex(header: BlockHeader) = {
    val blockId = blockUtils.getBlockId(header)
    storeFacade
      .getVertexByField[F](
        blockHeaderSchema.name,
        "blockId",
        blockId
      )
      .map(
        _.toRight(
          Failures.NoCurrentHeaderVertexFailure(ByteVector(blockId))
        )
      )
  }

  private def fetchPreviousHeaderVertex(header: BlockHeader) = {
    val blockId = blockUtils.getParentBlockId(header)
    storeFacade
      .getVertexByField[F](
        blockHeaderSchema.name,
        "blockId",
        blockId
      )
      .map(
        _.toRight(
          Failures.NoPreviousHeaderVertexFailure(ByteVector(blockId))
        )
      )
  }

  private def fetchBodyVertex(body: BlockBody) = {
    val transactions = blockUtils.blockBodyToByteArray(body)
    storeFacade
      .getVertexByField[F](
        blockBodySchema.name,
        "transactionIds",
        transactions
      )
      .map(
        _.toRight(
          Failures.NoCurrentBodyVertexFailure(ByteVector(transactions))
        )
      )
  }

}
