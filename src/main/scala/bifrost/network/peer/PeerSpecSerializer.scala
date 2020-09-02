package bifrost.network.peer

import java.net.{InetAddress, InetSocketAddress}

import bifrost.settings.VersionSerializer
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import bifrost.utils.Extensions._

class PeerSpecSerializer(featureSerializers: PeerFeature.Serializers) extends BifrostSerializer[PeerSpec] {
  override def serialize(obj: PeerSpec, w: Writer): Unit = {

    w.putByteString(obj.agentName)
    VersionSerializer.serialize(obj.protocolVersion, w)
    w.putByteString(obj.nodeName)

    w.putOption(obj.declaredAddress) { (writer, isa) =>
      val addr = isa.getAddress.getAddress
      writer.put((addr.size + 4).toByteExact)
      writer.putBytes(addr)
      writer.putUInt(isa.getPort)
    }

    w.put(obj.features.size.toByteExact)
    obj.features.foreach { f =>
      w.put(f.featureId)
      val fBytes = f.bytes
      w.putUShort(fBytes.length.toShortExact)
      w.putBytes(fBytes)
    }
  }

  override def parse(r: Reader): PeerSpec = {

    val appName = r.getByteString()
    require(appName.nonEmpty)

    val protocolVersion = VersionSerializer.parse(r)

    val nodeName = r.getByteString()

    val declaredAddressOpt = r.getOption {
      val fas = r.getUByte()
      val fa = r.getBytes(fas - 4)
      val port = r.getUInt().toIntExact
      new InetSocketAddress(InetAddress.getByAddress(fa), port)
    }

    val featuresCount = r.getByte()
    val feats = (1 to featuresCount).flatMap { _ =>
      val featId = r.getByte()
      val featBytesCount = r.getUShort().toShortExact
      val featChunk = r.getChunk(featBytesCount)
      //we ignore a feature found in the PeersData if we do not know how to parse it or failed to do that
      featureSerializers.get(featId).flatMap { featureSerializer =>
        featureSerializer.parseTry(r.newReader(featChunk)).toOption
      }
    }

    PeerSpec(appName, protocolVersion, nodeName, declaredAddressOpt, feats)
  }
}
