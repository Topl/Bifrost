package co.topl.genus.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.{Event, GroupId, SeriesId}
import co.topl.genus.algebras.{TokenFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genus.model.GE
import co.topl.genus.orientDb.instances.VertexSchemaInstances.instances._

object GraphTokenFetcher {

  def make[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, TokenFetcherAlgebra[F]] =
    Resource.pure {
      new TokenFetcherAlgebra[F] {

        def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[GroupPolicy]]] =
          EitherT(vertexFetcher.fetchGroupPolicy(groupId))
            .map(_.map(groupPolicySchema.decode))
            .value

        override def fetchSeriesPolicy(seriesId: SeriesId): F[Either[GE, Option[Event.SeriesPolicy]]] =
          EitherT(vertexFetcher.fetchSeriesPolicy(seriesId))
            .map(_.map(seriesPolicySchema.decode))
            .value

      }
    }
}
