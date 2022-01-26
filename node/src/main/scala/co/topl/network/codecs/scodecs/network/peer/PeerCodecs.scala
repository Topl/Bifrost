package co.topl.network.codecs.scodecs.network.peer

import co.topl.codecs._
import co.topl.network.peer.{LocalAddressPeerFeature, PeerFeature, PeerSpec}
import co.topl.network.codecs.scodecs.settings._
import co.topl.utils.Extensions.{IntOps, LongOps}
import scodec.Codec
import scodec.bits.BitVector
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
    (bytesCodec(LocalAddressPeerFeature.addressLength) :: uIntCodec)
      .xmapc { case fa :: port :: HNil =>
        LocalAddressPeerFeature(new InetSocketAddress(InetAddress.getByAddress(fa), port.toIntExact))
      }(feat => feat.address.getAddress.getAddress :: feat.address.getPort.toLong :: HNil)

  implicit val peerFeatureCodec: Codec[PeerFeature] =
    discriminated[PeerFeature]
      .by(byteCodec)
      .typecase(
        LocalAddressPeerFeature.featureId,
        uShortCodec
          .consume[LocalAddressPeerFeature](length =>
            bytesCodec(length)
              .exmapc[LocalAddressPeerFeature](bytes =>
                localAddressPeerFeatureCodec.decode(BitVector(bytes)).map(_.value)
              )(localAddress => localAddressPeerFeatureCodec.encode(localAddress).map(_.toByteArray))
          )(localAddress =>
            localAddressPeerFeatureCodec.encode(localAddress).map(_.length / 8).getOrElse(0L).toShortExact
          )
      )

  implicit val peerSpecCodec: Codec[PeerSpec] =
    (byteStringCodec ::
      versionCodec ::
      byteStringCodec ::
      optionCodec[InetSocketAddress] ::
      seqCodec[PeerFeature])
      .as[PeerSpec]
}
