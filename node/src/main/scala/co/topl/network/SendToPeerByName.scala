package co.topl.network

import akka.actor.ActorRef

import co.topl.network.peer.ConnectedPeer


case class SendToPeerByName(chosenPeer: String, routerRef:ActorRef) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = {
    val out = peers.filter(p => p.peerInfo.get.peerSpec.agentName == chosenPeer )
    if (out.isEmpty) {
//      println("Error: no peer found by name "+chosenPeer)
//      println("All peers:")
//      peers.foreach(p=>println(p.peerInfo.get.peerSpec.agentName))
      routerRef ! InvalidateHolders(chosenPeer)
    }
    out
  }
}