package co.topl.networking.blockchain

import cats.effect.Resource
import co.topl.networking.p2p.ConnectedPeer

trait BlockchainPeerHandlerAlgebra[F[_]] {

  /**
   * When there is a new P2P connection, invoke this method to handle the peer
   * @param client a client associated with a specific peer
   * @param connectedPeer connected peer
   * @return a "forever-running" void
   */
  def usePeer(client: BlockchainPeerClient[F], connectedPeer: ConnectedPeer): Resource[F, Unit]
}
