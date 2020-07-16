package bifrost.network

import bifrost.network.message.MessageSpec
import bifrost.network.peer.ConnectedPeer

import scala.reflect.ClassTag

// Messages shared by NetworkController, PeerSynchronizer and NodeViewSynchronizer
object SharedNetworkMessages {
  object ReceivableMessages {
    case class DataFromPeer[DT: ClassTag](spec: MessageSpec[DT], data: DT, source: ConnectedPeer)
  }
}
