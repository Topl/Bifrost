package co.topl.nodeCodecs.binary.legacy.network.peer

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.network.peer.{PeerFeature, PeerSpec}
import co.topl.settings.{Version, VersionSerializer}
import co.topl.utils.Extensions._

import java.net.{InetAddress, InetSocketAddress}

class PeerSpecSerializer(featureSerializers: PeerFeature.Serializers) extends BifrostSerializer[PeerSpec] {

  override def serialize(obj: PeerSpec, w: Writer): Unit = {
    /* agentName: String */
    w.putByteString(obj.agentName)

    /* version: Version */
    VersionSerializer.serialize(obj.version, w)

    /* nodeName: String */
    w.putByteString(obj.nodeName)

    /* declaredAddress: Option[InetSocketAddress] */
    w.putOption(obj.declaredAddress) { (writer, isa) =>
      val addr = isa.getAddress.getAddress
      writer.put((addr.size + 4).toByteExact)
      writer.putBytes(addr)
      writer.putUInt(isa.getPort)
    }

    /* features: Seq[PeerFeature] */
    w.put(obj.features.size.toByteExact)
    obj.features.foreach { f =>
      w.put(f.featureId)
      val fBytes = f.bytes
      w.putUShort(fBytes.length.toShortExact)
      w.putBytes(fBytes)
    }
  }

  override def parse(r: Reader): PeerSpec = {

    val appName: String = r.getByteString()
    require(appName.nonEmpty)

    val version: Version = VersionSerializer.parse(r)

    val nodeName: String = r.getByteString()

    val declaredAddressOpt: Option[InetSocketAddress] = r.getOption {
      val fas = r.getUByte()
      val fa = r.getBytes(fas - 4)
      val port = r.getUInt().toIntExact
      new InetSocketAddress(InetAddress.getByAddress(fa), port)
    }

    val featuresCount: Int = r.getByte()
    val feats: Seq[PeerFeature] = (1 to featuresCount).flatMap { _ =>
      val featId = r.getByte()
      val featBytesCount = r.getUShort().toShortExact
      val featChunk = r.getChunk(featBytesCount)
      //we ignore a feature found in the PeersData if we do not know how to parse it or failed to do that
      featureSerializers.get(featId).flatMap { featureSerializer =>
        featureSerializer.parseTry(r.newReader(featChunk)).toOption
      }
    }

    PeerSpec(appName, version, nodeName, declaredAddressOpt, feats)
  }
}
