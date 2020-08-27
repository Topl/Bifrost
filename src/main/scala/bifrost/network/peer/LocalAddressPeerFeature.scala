package bifrost.network.peer

import java.net.{InetAddress, InetSocketAddress}

import bifrost.utils.serialization.{BifrostSerializer, _}
import bifrost.utils.Extensions._

case class LocalAddressPeerFeature(address: InetSocketAddress) extends PeerFeature {
  override type M = LocalAddressPeerFeature
  override val featureId: PeerFeature.Id = LocalAddressPeerFeature.featureId

  def serializer: LocalAddressPeerFeatureSerializer.type = LocalAddressPeerFeatureSerializer
}

object LocalAddressPeerFeature {
  val featureId: PeerFeature.Id = 2: Byte
}

object LocalAddressPeerFeatureSerializer extends BifrostSerializer[LocalAddressPeerFeature] {

  private val AddressLength = 4

  override def serialize(obj: LocalAddressPeerFeature, w: Writer): Unit = {
    w.putBytes(obj.address.getAddress.getAddress)
    w.putUInt(obj.address.getPort)
  }

  override def parse(r: Reader): LocalAddressPeerFeature = {
    val fa = r.getBytes(AddressLength)
    val port = r.getUInt().toIntExact
    LocalAddressPeerFeature(new InetSocketAddress(InetAddress.getByAddress(fa), port))
  }
}