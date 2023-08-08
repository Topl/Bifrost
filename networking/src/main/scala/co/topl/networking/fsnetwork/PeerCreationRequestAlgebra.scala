package co.topl.networking.fsnetwork

import co.topl.networking.p2p.DisconnectedPeer

abstract class PeerCreationRequestAlgebra[F[_]] {
  def requestNewPeerCreation(peer: HostId): F[Unit]
}

object PeerCreationRequestAlgebra {

  def apply[F[_]](peerCreationFun: DisconnectedPeer => F[Unit]): PeerCreationRequestAlgebra[F] =
    (peer: HostId) => peerCreationFun(DisconnectedPeer(peer, (0, 0)))
}
