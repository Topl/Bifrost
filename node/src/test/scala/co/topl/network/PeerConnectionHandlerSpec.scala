package co.topl.network

import akka.actor._
import akka.actor.typed.scaladsl.adapter._
import akka.io.{IO, Tcp}
import akka.testkit.TestKit
import co.topl.network.codecs.legacy.message.TransmissionSerializer
import co.topl.network.utils.NetworkTimeProvider
import co.topl.nodeView.ValidTransactionGenerators
import co.topl.utils.{TestSettings, TimeProvider}
import org.scalatest.matchers.must.Matchers
import org.scalatest.propspec.AnyPropSpecLike

import java.net.InetSocketAddress

class PeerConnectionHandlerSpec
    extends TestKit(ActorSystem("PCHSpec"))
    with AnyPropSpecLike
    with Matchers
    with TestSettings
    with ValidTransactionGenerators {

  implicit val timeProvider: TimeProvider = new NetworkTimeProvider(settings.ntp)(system.toTyped)

  property("MessageSerializer should initialize correctly with specified message codes") {

    new TransmissionSerializer(settings.network.magicBytes)
  }

  property("A new PeerConnectionHandler should be created") {

    val peerManagerRef: ActorRef = system.actorOf(PeerManagerRef.props(settings, None))
    val networkControllerRef: ActorRef =
      system.actorOf(NetworkControllerRef.props(settings, peerManagerRef, appContext, IO(Tcp)))

    val localPort = 9085
    val remotePort = 9086
    val connectionId = ConnectionId(new InetSocketAddress(localPort), new InetSocketAddress(remotePort), Incoming)

    val connectionDescription = ConnectionDescription(networkControllerRef, connectionId, None, Seq())

    system.actorOf(PeerConnectionHandlerRef.props(networkControllerRef, settings, appContext, connectionDescription))
  }
}
