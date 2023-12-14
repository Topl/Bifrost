package co.topl.genus.algebras

import co.topl.algebras.ToplRpc

trait ChainReplicatorAlgebra[F[_], Stream[_]] {

  /**
   * Uses the given ToplRpc client to replicate the serving node's
   * blockchain data into some other external source.
   *
   * This is expected to be a never-terminating result.
   */
  def replicateFrom(toplRpc: ToplRpc[F, Stream]): F[Unit]

}
