package co.topl.grpc.services

import cats.implicits._
import cats.effect.Async
import co.topl.algebras.AdminRpc
import co.topl.node.services.{FetchSoftwareVersionReq, FetchSoftwareVersionRes, NodeAdminRpcFs2Grpc}
import fs2.Stream
import io.grpc.Metadata

class AdminService[F[_]: Async](interpreter: AdminRpc[F, Stream[F, *]]) extends NodeAdminRpcFs2Grpc[F, Metadata] {

  override def fetchSoftwareVersion(request: FetchSoftwareVersionReq, ctx: Metadata): F[FetchSoftwareVersionRes] =
    interpreter.fetchSoftwareVersion().map(FetchSoftwareVersionRes(_))

}
