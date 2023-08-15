package co.topl.blockchain

import co.topl.networking.p2p.RemoteAddress

class CurrentPeersHolder {
  private var hotPeersSet: Set[RemoteAddress] = Set.empty

  def hotPeersUpdate(update: Set[RemoteAddress]): Unit =
    hotPeersSet = update

  def hotPeers(): Set[RemoteAddress] = hotPeersSet
}
