package co.topl.blockchain

import cats.data.OptionT
import cats.effect.{Async, Resource}
import co.topl.algebras.AdminRpc
import co.topl.blockchain.algebras.NodeMetadataAlgebra
import fs2.Stream

object ToplAdminRpcServer {

  def make[F[_]: Async](nodeMetadataAlgebra: NodeMetadataAlgebra[F]): Resource[F, AdminRpc[F, Stream[F, *]]] =
    Resource.pure {
      new AdminRpc[F, Stream[F, *]] {
        override def fetchSoftwareVersion(): F[String] =
          OptionT(nodeMetadataAlgebra.readAppVersion).getOrElse("Undefined")
      }
    }

}
