package bifrost.network

import java.net._

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.io.{IO, Tcp}
import akka.pattern.ask
import akka.util.Timeout
import bifrost.network.message.Message
import bifrost.network.peer._
import bifrost.settings.{BifrostContext, NetworkSettings, Version}
import bifrost.utils.{Logging, NetworkUtils}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Control all network interaction
  * must be singleton
  */
class NetworkController(
    settings: NetworkSettings,
    peerManagerRef: ActorRef,
    bifrostContext: BifrostContext,
    tcpManager: ActorRef
)(implicit ec: ExecutionContext)
    extends Actor
    with Logging {

  // Import the types of messages this actor can RECEIVE
  import NetworkController.ReceivableMessages._

  // Import the types of messages this actor can SEND
  import NodeViewSynchronizer.ReceivableMessages.{DisconnectedPeer, HandshakedPeer}
  import PeerConnectionHandler.ReceivableMessages.CloseConnection
  import PeerManager.ReceivableMessages._
  import SharedNetworkMessages.ReceivableMessages.DataFromPeer

  private lazy val bindAddress = settings.bindAddress
  private implicit val system: ActorSystem = context.system
  private implicit val timeout: Timeout = Timeout(settings.controllerTimeout.getOrElse(5 seconds))
  override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = NetworkController.ChildActorHandlingRetriesNr,
    withinTimeRange = 1.minute
  ) {
    case _: ActorKilledException => Stop
    case _: DeathPactException   => Stop
    case e: ActorInitializationException =>
      log.warn(s"Stopping child actor failed with: $e")
      Stop
    case e: Exception =>
      log.warn(s"Restarting child actor failed with: $e")
      Restart
  }

  private var messageHandlers = Map.empty[message.Message.MessageCode, ActorRef]
  private var connections = Map.empty[InetSocketAddress, ConnectedPeer]
  private var unconfirmedConnections = Set.empty[InetSocketAddress]

  override def preStart(): Unit = {
    log.info(s"Declared address: ${bifrostContext.externalNodeAddress}")
    validateDeclaredAddress() //check own declared address for validity
    context become initialization
  }

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive = nonsense // should never get into this state

  private def initialization: Receive =
    bindingLogic orElse
      registerHandlers orElse
      nonsense

  private def operational: Receive =
    businessLogic orElse
      peerCommands orElse
      connectionEvents orElse
      nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS
  private def bindingLogic: Receive = {
    case BindP2P =>
      // send a bind signal to the TCP manager to designate this actor as the handler to accept incoming connections
      sender() ! (tcpManager ? Tcp.Bind(self, bindAddress, options = Nil, pullMode = false))

    case BecomeOperational =>
      log.info(s"${Console.YELLOW}Network Controller transitioning to the operational state${Console.RESET}")
      scheduleConnectionToPeer()
      context become operational
  }

  private def registerHandlers: Receive = {
    case RegisterMessageSpecs(specs, handler) =>
      log.info(
        s"${Console.YELLOW}Registered ${sender()} as the handler for ${specs.map(s => s.messageCode -> s.messageName)}${Console.RESET}"
      )
      messageHandlers ++= specs.map(_.messageCode -> handler)
  }

  private def businessLogic: Receive = {
    //a message coming in from another peer
    case message.Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.parseBytes(msgBytes) match {
        case Success(content) =>
          messageHandlers.get(msgId) match {
            case Some(handler) =>
              handler ! DataFromPeer(spec, content, remote)

            case None =>
              log.error(s"No handlers found for message $remote: " + msgId)
          }
        case Failure(e) =>
          log.error(s"Failed to deserialize data from $remote: ", e)
          penalize(
            remote.connectionId.remoteAddress,
            PenaltyType.PermanentPenalty
          )
      }

    case SendToNetwork(msg: message.Message[_], sendingStrategy) =>
      filterConnections(sendingStrategy, msg.spec.protocolVersion).foreach {
        connectedPeer => connectedPeer.handlerRef ! msg
      }

  }

  private def peerCommands: Receive = {
    case ConnectTo(peer) =>
      connectTo(peer)
      
    case DisconnectFrom(peer) =>
      log.info(s"Disconnected from ${peer.connectionId}")
      peer.handlerRef ! CloseConnection

    case GetConnectedPeers =>
      sender() ! connections.values.flatMap(_.peerInfo).toSeq

    case PenalizePeer(peerAddress, penaltyType) =>
      penalize(peerAddress, penaltyType)

    case Blacklisted(peerAddress) =>
      closeConnection(peerAddress)
  }

  private def connectionEvents: Receive = {
    case Tcp.Connected(remoteAddress, localAddress) if connectionForPeerAddress(remoteAddress).isEmpty =>
      val connectionDirection: ConnectionDirection =
        if (unconfirmedConnections.contains(remoteAddress)) Outgoing
        else Incoming
      val connectionId = ConnectionId(remoteAddress, localAddress, connectionDirection)
      log.info(s"Unconfirmed connection: ($remoteAddress, $localAddress) => $connectionId")

      if (connectionDirection.isOutgoing)
        createPeerConnectionHandler(connectionId, sender())
      else peerManagerRef ! ConfirmConnection(connectionId, sender())


    case Tcp.Connected(remoteAddress, _) =>
      log.warn(s"Connection to peer $remoteAddress is already established")
      sender() ! Tcp.Close

    case ConnectionConfirmed(connectionId, handlerRef) =>
      log.info(s"Connection confirmed to $connectionId")
      createPeerConnectionHandler(connectionId, handlerRef)

    case ConnectionDenied(connectionId, handlerRef) =>
      log.info(s"Incoming connection from ${connectionId.remoteAddress} denied")
      handlerRef ! Tcp.Close

    case Handshaked(connectedPeer) =>
      handleHandshake(connectedPeer, sender())

    case f @ Tcp.CommandFailed(c: Tcp.Connect) =>
      unconfirmedConnections -= c.remoteAddress
      f.cause match {
        case Some(t) => log.info("Failed to connect to : " + c.remoteAddress, t)
        case None    => log.info("Failed to connect to : " + c.remoteAddress)
      }
      // remove not responding peer from database
      peerManagerRef ! RemovePeer(c.remoteAddress)

    case Terminated(ref) =>
      connectionForHandler(ref).foreach { connectedPeer =>
        val remoteAddress = connectedPeer.connectionId.remoteAddress
        connections -= remoteAddress
        unconfirmedConnections -= remoteAddress
        context.system.eventStream.publish(DisconnectedPeer(remoteAddress))
      }

    case _: Tcp.ConnectionClosed =>
      log.info("Denied connection has been closed")
  }

  private def nonsense: Receive = {
    case Tcp.CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController (in context ${context.toString}): got unexpected input $nonsense from ${sender()}")
  }

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /**
    * Schedule a periodic connection to a random known peer
    */
  private def scheduleConnectionToPeer(): Unit = {
    context.system.scheduler.schedule(5.seconds, 5.seconds) {
      if (connections.size < settings.maxConnections) {
        val randomPeerF = peerManagerRef ? RandomPeerExcluding(
          connections.values.flatMap(_.peerInfo).toSeq
        )
        randomPeerF.mapTo[Option[PeerInfo]].foreach { peerInfoOpt =>
          peerInfoOpt.foreach(peerInfo => self ! ConnectTo(peerInfo))
        }
      }
    }
  }

  /**
    * Connect to peer
    *
    * @param peer - PeerInfo
    */
  private def connectTo(peer: PeerInfo): Unit = {
    log.info(s"Connecting to peer: $peer")
    getPeerAddress(peer) match {
      case Some(remote) =>
        if (connectionForPeerAddress(remote).isEmpty && !unconfirmedConnections
              .contains(remote)) {
          unconfirmedConnections += remote
          tcpManager ! Tcp.Connect(
            remoteAddress = remote,
            options = Nil,
            timeout = Some(settings.connectionTimeout),
            pullMode = true
          )
        } else {
          log.warn(s"Connection to peer $remote is already established")
        }
      case None =>
        log.warn(s"Can't obtain remote address for peer $peer")
    }
  }

  /**
    * Creates a PeerConnectionHandler for the established connection
    *
    * @param connectionId - connection detailed info
    * @param connection - connection ActorRef
    */
  private def createPeerConnectionHandler(
      connectionId: ConnectionId,
      connection: ActorRef
  ): Unit = {
    log.info {
      connectionId.direction match {
        case Incoming =>
          s"New incoming connection from ${connectionId.remoteAddress} established (bound to local ${connectionId.localAddress})"
        case Outgoing =>
          s"New outgoing connection to ${connectionId.remoteAddress} established (bound to local ${connectionId.localAddress})"
      }
    }

    val isLocal = connectionId.remoteAddress.getAddress.isSiteLocalAddress ||
      connectionId.remoteAddress.getAddress.isLoopbackAddress
    val peerFeatures =
      if (isLocal)
        bifrostContext.features :+ LocalAddressPeerFeature(
          new InetSocketAddress(
            connectionId.localAddress.getAddress,
            settings.bindAddress.getPort
          )
        )
      else bifrostContext.features

    val selfAddressOpt = getNodeAddressForPeer(connectionId.localAddress)

    if (selfAddressOpt.isEmpty)
      log.warn(
        "Unable to define external address. Specify it manually in `scorex.network.declaredAddress`."
      )

    val connectionDescription = ConnectionDescription(
      connection,
      connectionId,
      selfAddressOpt,
      peerFeatures
    )

    val handler: ActorRef = peer.PeerConnectionHandlerRef(
      settings,
      self,
      bifrostContext,
      connectionDescription
    )
    context.watch(handler)
    val connectedPeer = peer.ConnectedPeer(connectionId, handler, None)
    connections += connectionId.remoteAddress -> connectedPeer
    unconfirmedConnections -= connectionId.remoteAddress
  }

  private def handleHandshake(
      peerInfo: PeerInfo,
      peerHandlerRef: ActorRef
  ): Unit = {
    connectionForHandler(peerHandlerRef).foreach { connectedPeer =>
      val remoteAddress = connectedPeer.connectionId.remoteAddress
      val peerAddress = peerInfo.peerSpec.address.getOrElse(remoteAddress)

      //drop connection to self if occurred or peer already connected
      val shouldDrop = isSelf(remoteAddress) ||
        connectionForPeerAddress(peerAddress).exists(
          _.handlerRef != peerHandlerRef
        )
      if (shouldDrop) {
        connectedPeer.handlerRef ! CloseConnection
        peerManagerRef ! RemovePeer(peerAddress)
        connections -= connectedPeer.connectionId.remoteAddress
      } else {
        peerManagerRef ! AddOrUpdatePeer(peerInfo)

        val updatedConnectedPeer = connectedPeer.copy(peerInfo = Some(peerInfo))
        connections += remoteAddress -> updatedConnectedPeer
        context.system.eventStream.publish(HandshakedPeer(updatedConnectedPeer))
      }
    }
  }

  /**
    * Returns connections filtered by given SendingStrategy and Version.
    * Exclude all connections with lower version and apply sendingStrategy to remaining connected peers
    *
    * @param sendingStrategy - SendingStrategy
    * @param version         - minimal version required
    * @return sequence of ConnectedPeer instances according SendingStrategy
    */
  private def filterConnections(
      sendingStrategy: SendingStrategy,
      version: Version
  ): Seq[ConnectedPeer] = {
    sendingStrategy.choose(
      connections.values.toSeq
        .filter(_.peerInfo.exists(_.peerSpec.protocolVersion >= version))
    )
  }

  /**
    * Returns connection for given PeerConnectionHandler ActorRef
    *
    * @param handler ActorRef on PeerConnectionHandler actor
    * @return Some(ConnectedPeer) when the connection exists for this handler, and None otherwise
    */
  private def connectionForHandler(handler: ActorRef) = {
    connections.values.find { connectedPeer =>
      connectedPeer.handlerRef == handler
    }
  }

  /**
    * Returns connection for given address of the peer
    *
    * @param peerAddress - socket address of peer
    * @return Some(ConnectedPeer) when the connection exists for this peer, and None otherwise
    */
  private def connectionForPeerAddress(peerAddress: InetSocketAddress) = {
    connections.values.find { connectedPeer =>
      connectedPeer.connectionId.remoteAddress == peerAddress ||
      connectedPeer.peerInfo.exists(peerInfo =>
        getPeerAddress(peerInfo).contains(peerAddress)
      )
    }
  }

  /**
    * Checks the node owns the address
    */
  private def isSelf(peerAddress: InetSocketAddress): Boolean = {
    NetworkUtils.isSelf(
      peerAddress,
      bindAddress,
      bifrostContext.externalNodeAddress
    )
  }

  /**
    * Returns local address of peer for local connections and WAN address of peer for
    * external connections. When local address is not known, try to ask it at the UPnP gateway
    *
    * @param peer - known information about peer
    * @return socket address of the peer
    */
  private def getPeerAddress(peer: PeerInfo): Option[InetSocketAddress] = {
    (peer.peerSpec.localAddressOpt, peer.peerSpec.declaredAddress) match {
      case (Some(localAddr), _) =>
        Some(localAddr)

      case (None, Some(declaredAddress)) if bifrostContext.externalNodeAddress.exists(_.getAddress == declaredAddress.getAddress) =>
        bifrostContext.upnpGateway.flatMap(_.getLocalAddressForExternalPort(declaredAddress.getPort))

      case _ => peer.peerSpec.declaredAddress
    }
  }

  /**
    * Returns the node address reachable from Internet
    *
    * @param localSocketAddress - local socket address of the connection to the peer
    * @return - socket address of the node
    */
  private def getNodeAddressForPeer(localSocketAddress: InetSocketAddress) = {
    val localAddr = localSocketAddress.getAddress
    bifrostContext.externalNodeAddress match {
      case Some(extAddr) =>
        Some(extAddr)

      case None =>
        if (!localAddr.isSiteLocalAddress && !localAddr.isLoopbackAddress
            && localSocketAddress.getPort == settings.bindAddress.getPort) {
          Some(localSocketAddress)
        } else {
          val listenAddrs = NetworkUtils
            .getListenAddresses(settings.bindAddress)
            .filterNot(addr =>
              addr.getAddress.isSiteLocalAddress || addr.getAddress.isLoopbackAddress
            )

          listenAddrs
            .find(addr => localAddr == addr.getAddress)
            .orElse(listenAddrs.headOption)
        }
    }
  }

  private def validateDeclaredAddress(): Unit = {
    if (!settings.localOnly) {
      settings.declaredAddress.foreach { mySocketAddress =>
        Try {
          val uri = new URI("http://" + mySocketAddress)
          val myHost = uri.getHost
          val myAddress = InetAddress.getAllByName(myHost)

          val listenAddresses = NetworkUtils.getListenAddresses(bindAddress)
          val upnpAddress = bifrostContext.upnpGateway.map(_.externalAddress)

          val valid =
            listenAddresses.exists(myAddress.contains) || upnpAddress.exists(
              myAddress.contains
            )

          if (!valid) {
            log.error(s"""Declared address validation failed:
                 | $mySocketAddress not match any of the listening address: $listenAddresses
                 | or Gateway WAN address: $upnpAddress""".stripMargin)
          }
        } recover {
          case t: Throwable =>
            log.error("Declared address validation failed: ", t)
        }
      }
    }
  }

  private def closeConnection(peerAddress: InetSocketAddress): Unit =
    connections.get(peerAddress).foreach { peer =>
      connections = connections.filterNot {
        case (address, _) => // clear all connections related to banned peer ip
          Option(peer.connectionId.remoteAddress.getAddress)
            .exists(Option(address.getAddress).contains(_))
      }
      peer.handlerRef ! CloseConnection
    }

  /**
    * Register a new penalty for given peer address.
    */
  private def penalize(
      peerAddress: InetSocketAddress,
      penaltyType: PenaltyType
  ): Unit =
    peerManagerRef ! Penalize(peerAddress, penaltyType)

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object NetworkController {

  val ChildActorHandlingRetriesNr: Int = 10

  object ReceivableMessages {

    case class Handshaked(peer: PeerInfo)

    case class RegisterMessageSpecs(specs: Seq[message.MessageSpec[_]], handler: ActorRef)

    case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

    case class ConnectTo(peer: PeerInfo)

    case class DisconnectFrom(peer: ConnectedPeer)

    case class PenalizePeer(address: InetSocketAddress, penaltyType: PenaltyType)

    case class ConnectionConfirmed(connectionId: ConnectionId, handlerRef: ActorRef)

    case class ConnectionDenied(connectionId: ConnectionId, handlerRef: ActorRef)

    case object ShutdownNetwork

    case object GetConnectedPeers

    case object BindP2P

    case object BecomeOperational

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object NetworkControllerRef {
  def apply(settings: NetworkSettings, peerManagerRef: ActorRef, bifrostContext: BifrostContext
           )(implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(
      props(settings, peerManagerRef, bifrostContext, IO(Tcp))
    )
  }

  def apply(name: String, settings: NetworkSettings, peerManagerRef: ActorRef, bifrostContext: BifrostContext, tcpManager: ActorRef
           )(implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(
      props(settings, peerManagerRef, bifrostContext, tcpManager),
      name
    )
  }

  def apply(
      name: String,
      settings: NetworkSettings,
      peerManagerRef: ActorRef,
      bifrostContext: BifrostContext
  )(implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(
      props(settings, peerManagerRef, bifrostContext, IO(Tcp)),
      name
    )
  }

  def props(
      settings: NetworkSettings,
      peerManagerRef: ActorRef,
      bifrostContext: BifrostContext,
      tcpManager: ActorRef
  )(implicit ec: ExecutionContext): Props = {
    Props(
      new NetworkController(
        settings,
        peerManagerRef,
        bifrostContext,
        tcpManager
      )
    )
  }
}
