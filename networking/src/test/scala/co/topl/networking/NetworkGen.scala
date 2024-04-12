package co.topl.networking

import co.topl.networking.blockchain.NetworkProtocolVersions
import co.topl.networking.p2p._
import com.google.protobuf.ByteString
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait NetworkGen {

  implicit val arbitraryRemoteAddress: Arbitrary[RemoteAddress] =
    Arbitrary(Gen.chooseNum[Int](0, 65535).map(port => RemoteAddress("localhost", port)))

  implicit val arbitraryConnectedPeer: Arbitrary[ConnectedPeer] =
    Arbitrary(
      for {
        address <- arbitraryRemoteAddress.arbitrary
        peerVK  <- Gen.containerOfN[Array, Byte](32, Arbitrary.arbByte.arbitrary)
      } yield ConnectedPeer(address, ByteString.copyFrom(peerVK), NetworkProtocolVersions.V1)
    )

}

object NetworkGen extends NetworkGen
