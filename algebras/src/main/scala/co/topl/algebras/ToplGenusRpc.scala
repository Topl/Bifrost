package co.topl.algebras

import co.topl.genus.services.BlockResponse

/**
 * Topl Genus Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 *
 * @tparam F Effect type
 * @tparam S Canonical head changes Synchronization Traversal Container, Ex: Stream, Seq
 */

/**
 * TODO Genus Rpc is used by GenusGrpc Client Implementation, which is used only right now by Byzantine test
 * In future PRs, we should complete the Client Implementation, and this trait
 */
trait ToplGenusRpc[F[_]] {
  def blockIdAtHeight(height: Long): F[BlockResponse]
}
