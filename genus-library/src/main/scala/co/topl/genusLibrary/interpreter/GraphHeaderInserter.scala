package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.genusLibrary.algebras.HeaderInserter
import co.topl.genusLibrary.algebras.mediator.HeaderMediatorAlgebra
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.failure.Failures.OrientCommitException
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.StoreFacade
import co.topl.genusLibrary.orientDb.GenusGraphMetadata._
import org.typelevel.log4cats.Logger
import scala.util.Try

class GraphHeaderInserter[F[_]: Async : Logger](
  orientDB:       StoreFacade,
  headerMediator: HeaderMediatorAlgebra[F]
) extends HeaderInserter[F] {

  override def insert(block: BlockData): F[Either[Failure, Unit]] = {
    val graph = orientDB.getGraph[F]

    (for {
      _ <- EitherT(graph.withEffectfulTransaction {
        graph.createVertex(block.header).asRight[Failure].pure[F]
      })
        .semiflatTap(_ => Logger[F].debug(s"Graph header inserter block ${block.header.slot}"))

//      _ <- EitherT.fromEither[F](
//        Try(graph.createVertex(block.header))
//        .map(_ => Right(()))
//          .getOrElse[Either[Failure, Unit]](Left(OrientCommitException(new IllegalStateException("replace with this code"))))
//      )
      mediation <- EitherT(headerMediator.mediate(block))
    } yield mediation).value
  }

}
