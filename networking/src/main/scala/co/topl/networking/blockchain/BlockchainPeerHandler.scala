package co.topl.networking.blockchain

trait BlockchainPeerHandler[F[_]] {

  /**
   * When there is a new P2P connection, invoke this method to handle the peer
   * @param client a client associated with a specific peer
   * @return a "forever-running" void
   */
  def usePeer(client: BlockchainPeerClient[F]): F[Unit]
}
