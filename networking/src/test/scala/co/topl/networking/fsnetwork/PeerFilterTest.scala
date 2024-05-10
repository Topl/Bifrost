package co.topl.networking.fsnetwork

import co.topl.models.p2p._
import co.topl.networking.fsnetwork._
import com.google.protobuf.ByteString
import munit.FunSuite

class PeerFilterTest extends FunSuite {

  private def ipToPeer(ip: String) = RemotePeer(HostId(ByteString.copyFrom("host".getBytes)), RemoteAddress(ip, 0))

  test("non ip address is not filtered by default") {
    val underTest = new PeerFilter(Seq("10.45.67.89", "9.45.67.89/32"), Seq.empty)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("wrongIpAddress")), true)
  }

  test("HotPeers filter IP single addresses") {
    val underTest = new PeerFilter(Seq("10.45.67.89", "9.45.67.89/32"), Seq.empty)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.89")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.90")), true)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("9.45.67.89")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("9.45.67.90")), true)
  }

  test("HotPeers filter IP based on subnet 1") {
    val underTest = new PeerFilter(Seq("10.45.67.0/24"), Seq.empty)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.89")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.0")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.255")), false)

    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.68.89")), true)
  }

  test("HotPeers filter IP based on subnet 2") {
    val underTest = new PeerFilter(Seq("10.45.67.0/255.255.255.0"), Seq.empty)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.89")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.0")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.255")), false)

    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.68.89")), true)
  }

  test("HotPeers filter IP based on mask character") {
    val underTest = new PeerFilter(Seq("10.45.67.*"), Seq.empty)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.89")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.0")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.255")), false)

    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.68.89")), true)
  }

  test("HotPeers filter IP based on range") {
    val underTest = new PeerFilter(Seq("10.45.67.0-255"), Seq.empty)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.89")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.0")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.255")), false)

    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.68.89")), true)
  }

  test("HotPeers filter IP based on complex usage") {
    val underTest = new PeerFilter(Seq("10.*.65-67.0/24"), Seq.empty)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.67.1")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.0.65.80")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.255.66.200")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.64.255")), true)

    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.45.65.0")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.0.65.80")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.0.65.200")), false)
    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("10.0.68.255")), true)

    assertEquals(underTest.remotePeerIsAcceptable(ipToPeer("34.45.68.89")), true)
  }

  private def stringToHostId(id:     String) = HostId(ByteString.copyFrom(id.getBytes))
  private def stringToRemotePeer(id: String) = RemotePeer(stringToHostId(id), RemoteAddress("", 0))

  test("HotPeers filter peerId") {
    val underTest = new PeerFilter(Seq.empty, Seq(stringToHostId("first"), stringToHostId("second")))
    assertEquals(underTest.remotePeerIsAcceptable(stringToRemotePeer("first")), false)
    assertEquals(underTest.remotePeerIsAcceptable(stringToRemotePeer("not-first")), true)
    assertEquals(underTest.remotePeerIsAcceptable(stringToRemotePeer("second")), false)
    assertEquals(underTest.remotePeerIsAcceptable(stringToRemotePeer("not-second")), true)
  }
}
