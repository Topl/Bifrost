package co.topl.network

import akka.actor._
import akka.testkit.TestKit
import co.topl.network.message.MessageSerializer
import co.topl.settings.{AppContext, StartupOpts}
import co.topl.utils.CoreGenerators
import org.scalatest.matchers.must.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext.Implicits.global

class PeerConnectionHandlerSpec
    extends TestKit(ActorSystem("PCHSpec"))
    with AnyPropSpecLike
    with ScalaCheckPropertyChecks
    with Matchers
    with CoreGenerators {

  val appContext = new AppContext(settings, StartupOpts.empty, None)

  property("MessageSerializer should initialize correctly with specified message codes") {

    new MessageSerializer(appContext.messageSpecs, settings.network.magicBytes)
  }

  property("A new PeerConnectionHandler should be created") {

    val peerManagerRef: ActorRef = PeerManagerRef("peerManager", settings, appContext)
    val networkControllerRef: ActorRef =
      NetworkControllerRef("networkController", settings, peerManagerRef, appContext)

    val localPort = 9085
    val remotePort = 9086
    val connectionId = ConnectionId(new InetSocketAddress(localPort), new InetSocketAddress(remotePort), Incoming)

    val connectionDescription = ConnectionDescription(networkControllerRef, connectionId, None, Seq())

    PeerConnectionHandlerRef(networkControllerRef, settings, appContext, connectionDescription)
  }
}
