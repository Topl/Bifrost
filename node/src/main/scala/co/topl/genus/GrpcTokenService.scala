package co.topl.genus

import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.TokenFetcherAlgebra
import io.grpc.Metadata

class GrpcTokenService[F[_]: Async](
  tokenFetcherAlgebra: TokenFetcherAlgebra[F]
) extends TokenServiceFs2Grpc[F, Metadata] {

  override def getGroupPolicy(request: QueryByGroupIdRequest, ctx: Metadata): F[GroupPolicyResponse] =
    EitherT(tokenFetcherAlgebra.fetchGroupPolicy(request.groupId))
      .map(GroupPolicyResponse(_))
      .rethrowT
      .adaptErrorsToGrpc

  override def getSeriesPolicy(request: QueryBySeriesIdRequest, ctx: Metadata): F[SeriesPolicyResponse] =
    EitherT(tokenFetcherAlgebra.fetchSeriesPolicy(request.seriesId))
      .map(SeriesPolicyResponse(_))
      .rethrowT
      .adaptErrorsToGrpc
}
