package co.topl.nodeCodecs.binary.scodecs.network.peer

import co.topl.codecs._
import co.topl.network.peer.{LocalAddressPeerFeature, PeerFeature, PeerSpec}
import co.topl.nodeCodecs.binary.scodecs.settings._
import co.topl.utils.Extensions.{IntOps, LongOps}
import scodec.Codec
import scodec.codecs.discriminated
import shapeless.{::, HNil}

import java.net.{InetAddress, InetSocketAddress}

trait PeerCodecs {

  implicit val inetSocketAddressCodec: Codec[InetSocketAddress] =
    uByteCodec.consume[InetSocketAddress](fas =>
      (bytesCodec(fas - 4) :: uIntCodec)
        .xmapc[InetSocketAddress] { case fa :: port :: HNil =>
          new InetSocketAddress(InetAddress.getByAddress(fa), port.toIntExact)
        }(isa => isa.getAddress.getAddress :: isa.getPort.toLong :: HNil)
    )(isa => (isa.getAddress.getAddress.length + 4).toByteExact)

  implicit val localAddressPeerFeatureCodec: Codec[LocalAddressPeerFeature] =
    inetSocketAddressCodec.as[LocalAddressPeerFeature]

  implicit val peerFeatureCodec: Codec[PeerFeature] =
    discriminated[PeerFeature]
      .by(byteCodec)
      .typecase(LocalAddressPeerFeature.featureId, localAddressPeerFeatureCodec)

  implicit val peerSpecCodec: Codec[PeerSpec] =
    (byteStringCodec ::
      versionCodec ::
      byteStringCodec ::
      optionCodec[InetSocketAddress] ::
      listCodec[PeerFeature].as[Seq[PeerFeature]])
      .as[PeerSpec]
}
