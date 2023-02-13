package co.topl.networking

import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader, ConnectionLeaders, RemoteAddress}
import org.scalacheck.{Arbitrary, Gen}

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

  implicit val arbitraryConnectionLeader: Arbitrary[ConnectionLeader] =
    Arbitrary(
      Gen.oneOf(ConnectionLeaders.Local, ConnectionLeaders.Remote)
    )
}

object NetworkGen extends NetworkGen
