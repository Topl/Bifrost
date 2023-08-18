package co.topl.networking.fsnetwork

import co.topl.networking.p2p.{DisconnectedPeer, RemoteAddress}

abstract class PeerCreationRequestAlgebra[F[_]] {
  def requestNewPeerCreation(peer: RemoteAddress): F[Unit]
}

object PeerCreationRequestAlgebra {

  def apply[F[_]](peerCreationFun: DisconnectedPeer => F[Unit]): PeerCreationRequestAlgebra[F] =
    (peer: RemoteAddress) => peerCreationFun(DisconnectedPeer(peer, (0, 0)))
}
