package co.topl.networking

import co.topl.networking.p2p._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait NetworkGen {

  implicit val arbitraryRemoteAddress: Arbitrary[RemoteAddress] =
    Arbitrary(Gen.chooseNum[Int](0, 65535).map(port => RemoteAddress("localhost", port)))

  implicit val arbitraryCoordinate: Arbitrary[(Double, Double)] =
    Arbitrary(
      for {
        x <- Gen.chooseNum[Double](-180, 180)
        y <- Gen.chooseNum[Double](-180, 180)
      } yield (x, y)
    )

  implicit val arbitraryConnectedPeer: Arbitrary[ConnectedPeer] =
    Arbitrary(
      for {
        address    <- arbitraryRemoteAddress.arbitrary
        coordinate <- arbitraryCoordinate.arbitrary
      } yield ConnectedPeer(address, coordinate)
    )

  implicit val arbitrarySocketLeader: Arbitrary[SocketLeader] =
    Arbitrary(
      Gen.oneOf(SocketLeader.Local, SocketLeader.Remote)
    )
}

object NetworkGen extends NetworkGen
