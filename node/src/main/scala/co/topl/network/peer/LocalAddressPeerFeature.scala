package co.topl.network.peer

import co.topl.nodeCodecs.binary.legacy.network.peer.LocalAddressPeerFeatureSerializer

import java.net.InetSocketAddress

case class LocalAddressPeerFeature(address: InetSocketAddress) extends PeerFeature {
  override type M = LocalAddressPeerFeature
  override val featureId: PeerFeature.Id = LocalAddressPeerFeature.featureId

  def serializer: LocalAddressPeerFeatureSerializer.type = LocalAddressPeerFeatureSerializer
}

object LocalAddressPeerFeature {
  val featureId: PeerFeature.Id = 2: Byte

  val addressLength: Int = 4
}
