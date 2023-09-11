package co.topl.genus

import cats.data.EitherT
import cats.effect.kernel.Async
import co.topl.genus.services._
import co.topl.genusLibrary.algebras.ValueFetcherAlgebra
import co.topl.genusLibrary.model.GEs
import io.grpc.Metadata

class GrpcValueService[F[_]: Async](
  valueFetcherAlgebra: ValueFetcherAlgebra[F]
) extends ValueServiceFs2Grpc[F, Metadata] {

  override def getGroupPolicy(request: QueryByGroupIdRequest, ctx: Metadata): F[GroupPolicyResponse] =
    EitherT(valueFetcherAlgebra.fetchGroupPolicy(request.groupId))
      .map(GroupPolicyResponse(_))
      .rethrowT
      .adaptErrorsToGrpc

  override def getSeriesPolicy(request: QueryBySeriesIdRequest, ctx: Metadata): F[SeriesPolicyResponse] =
    Async[F].raiseError[SeriesPolicyResponse](GEs.UnImplemented).adaptErrorsToGrpc
}
