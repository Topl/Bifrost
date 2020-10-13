package co.topl.network

import java.net.InetSocketAddress

import akka.actor._
import akka.io.{IO, Tcp}
import akka.testkit.TestKit
import akka.testkit.TestActorRef
import bifrost.BifrostGenerators
import bifrost.network.peer.PeerConnectionHandlerRef
import bifrost.settings.BifrostContext
import org.scalatest.PrivateMethodTester
import org.scalatest.matchers.must.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.ExecutionContext.Implicits.global

class PeerConnectionHandlerSpec extends TestKit(ActorSystem("PCHSpec"))
  with AnyPropSpecLike
  with ScalaCheckPropertyChecks
  with PrivateMethodTester
  with Matchers
  with BifrostGenerators {

  property("A new PeerConnectionHandler should be created with the specified message codes") {

    val bifrostContext = new BifrostContext(settings, None)

    val peerManagerRef: ActorRef = peer.PeerManagerRef("peerManager", settings.network, bifrostContext)
    val networkControllerRef: ActorRef =
      NetworkControllerRef("networkController", settings.network, peerManagerRef, bifrostContext)

    val localPort = 9085
    val remotePort = 9086
    val connectionId = ConnectionId(new InetSocketAddress(localPort), new InetSocketAddress(remotePort), Incoming)

    val connectionDescription = ConnectionDescription(networkControllerRef, connectionId, None, Seq())

    val pch = PeerConnectionHandlerRef(settings.network, networkControllerRef, bifrostContext, connectionDescription)
  }
}
