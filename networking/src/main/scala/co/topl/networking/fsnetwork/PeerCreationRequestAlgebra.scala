package co.topl.networking.fsnetwork

import co.topl.networking.p2p.DisconnectedPeer

abstract class PeerCreationRequestAlgebra[F[_]] {
  def requestNewPeerCreation(disconnectedPeer: DisconnectedPeer): F[Unit]
}

object PeerCreationRequestAlgebra {

  def apply[F[_]](peerCreationFun: DisconnectedPeer => F[Unit]): PeerCreationRequestAlgebra[F] =
    (disconnectedPeer: DisconnectedPeer) => peerCreationFun(disconnectedPeer)
}
