package co.topl.nodeCodecs.binary.scodecs

import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.peer.PeerSpec
import co.topl.settings.Version
import org.scalacheck.Gen

import java.net.{InetAddress, InetSocketAddress}

object Generators {
  val byteGen: Gen[Byte] = Gen.oneOf((0 to 255).map(_.toByte))

  val inetSocketAddressGen: Gen[InetSocketAddress] =
    Gen.posNum[Int].filter(_ <= 9999).map(port => new InetSocketAddress(InetAddress.getLocalHost, port))

  val peerSpecGen: Gen[PeerSpec] =
    Gen
      .zip(
        Gen.asciiPrintableStr.filter(_.nonEmpty),
        Gen.zip(byteGen, byteGen, byteGen).map(bytes => new Version(bytes._1, bytes._2, bytes._3)),
        Gen.asciiPrintableStr.filter(_.nonEmpty),
        Gen.option(inetSocketAddressGen)
      )
      .map { case (agent, version, name, address) =>
        PeerSpec(agent, version, name, address, Seq())
      }

  val handshakeGen: Gen[MessagesV1.Handshake] =
    Gen.zip(peerSpecGen, Gen.posNum[Long]).map(values => MessagesV1.Handshake(values._1, values._2))

  val modifierTypeIdGen: Gen[ModifierTypeId] =
    Gen.oneOf((0 to 255).map(_.toByte)).map(x => ModifierTypeId(x))
}
