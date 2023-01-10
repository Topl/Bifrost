package co.topl.blockchain.network

object NetworkManager {
  def startNetwork[F[_]](networkAlgebra: NetworkAlgebra[F], initialHosts: List[HostId]) = {
    for {
      reputationAggregator <- networkAlgebra.makeReputationAggregation
      blockBodies <- networkAlgebra.makeBlockBodyChecker(reputationAggregator)
      blockHeaders <- networkAlgebra.makeBlockHeaderChecker(reputationAggregator, blockBodies)
      peerManager <- networkAlgebra.makePeerManger(networkAlgebra, reputationAggregator, blockHeaders, blockBodies)
      _ = reputationAggregator.sendNoWait(ReputationAggregator.Message.SetPeerManager(peerManager))
      _ = blockBodies.sendNoWait(BlockBodiesChecker.Message.SetPeerManager(peerManager))
      //TODO send initial host list to peer manager
    } yield peerManager
  }
}
