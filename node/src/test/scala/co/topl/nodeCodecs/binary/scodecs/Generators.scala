package co.topl.nodeCodecs.binary.scodecs

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.peer.{LocalAddressPeerFeature, PeerFeature, PeerSpec}
import co.topl.settings.Version
import io.netty.channel.local.LocalAddress
import org.scalacheck.Gen

import java.net.{InetAddress, InetSocketAddress}

object Generators {
  val byteGen: Gen[Byte] = Gen.oneOf((0 to 255).map(_.toByte))

  val tripleDigitNumGen: Gen[Int] =
    Gen.posNum[Int].filter(_ <= 999)

  val inetSocketAddressGen: Gen[InetSocketAddress] =
    Gen
      .zip(
        Gen.alphaLowerStr,
        tripleDigitNumGen,
        tripleDigitNumGen,
        tripleDigitNumGen,
        tripleDigitNumGen,
        Gen
          .posNum[Int]
          .filter(_ <= 9999)
      )
      .map { case (name, first, second, third, fourth, port) =>
        new InetSocketAddress(InetAddress.getByName(s"$first.$second.$third.$fourth"), port)
      }

  val peerFeatureGen: Gen[PeerFeature] = inetSocketAddressGen.map(a => LocalAddressPeerFeature(a))

  val peerSpecGen: Gen[PeerSpec] =
    Gen
      .zip(
        Gen.asciiPrintableStr.filter(_.nonEmpty),
        Gen.zip(byteGen, byteGen, byteGen).map(bytes => new Version(bytes._1, bytes._2, bytes._3)),
        Gen.asciiPrintableStr.filter(_.nonEmpty),
        Gen.option(inetSocketAddressGen),
        Gen.listOf(peerFeatureGen)
      )
      .map { case (agent, version, name, address, features) =>
        PeerSpec(agent, version, name, address, features)
      }

  val handshakeGen: Gen[MessagesV1.Handshake] =
    Gen.zip(peerSpecGen, Gen.posNum[Long]).map(values => MessagesV1.Handshake(values._1, values._2))

  val modifierTypeIdGen: Gen[ModifierTypeId] =
    Gen.oneOf((0 to 255).map(_.toByte)).map(x => ModifierTypeId(x))
}
