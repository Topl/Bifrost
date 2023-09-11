package co.topl.genusLibrary.interpreter

import cats.data.EitherT
import cats.effect.Resource
import cats.effect.kernel.Async
import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.GroupId
import co.topl.genusLibrary.algebras.{TokenFetcherAlgebra, VertexFetcherAlgebra}
import co.topl.genusLibrary.model.GE
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances._

object GraphTokenFetcher {

  def make[F[_]: Async](vertexFetcher: VertexFetcherAlgebra[F]): Resource[F, TokenFetcherAlgebra[F]] =
    Resource.pure {
      new TokenFetcherAlgebra[F] {
        def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[GroupPolicy]]] =
          EitherT(vertexFetcher.fetchGroupPolicy(groupId))
            .map(_.map(groupPolicySchema.decodeVertex))
            .value
      }
    }
}
