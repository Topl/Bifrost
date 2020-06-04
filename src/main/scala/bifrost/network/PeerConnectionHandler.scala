package bifrost.network

import akka.actor.{Actor, ActorRef, ActorSystem, Cancellable, Props, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import bifrost.settings.Version
import bifrost.network.NetworkController.ReceivableMessages.{Handshaked, PenalizePeer}
import bifrost.network.PeerConnectionHandler.ReceivableMessages
import bifrost.network.PeerFeature.Serializers
import bifrost.network.message.{HandshakeSpec, MessageSerializer}
import bifrost.network.peer.{PeerInfo, PenaltyType}
import bifrost.settings.BifrostContext
import bifrost.settings.NetworkSettings
import bifrost.utils.Logging
import bifrost.utils.serialization.BifrostSerializer

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class PeerConnectionHandler(val settings: NetworkSettings,
                            networkControllerRef: ActorRef,
                            bifrostContext: BifrostContext,
                            connectionDescription: ConnectionDescription
                           )(implicit ec: ExecutionContext)
  extends Actor with Logging {

  import PeerConnectionHandler.ReceivableMessages._

  private val connection = connectionDescription.connection
  private val connectionId = connectionDescription.connectionId
  private val direction = connectionDescription.connectionId.direction
  private val ownSocketAddress = connectionDescription.ownSocketAddress
  private val localFeatures = connectionDescription.localFeatures

  private val featureSerializers: Serializers =
    localFeatures.map(f => f.featureId -> (f.serializer: BifrostSerializer[_ <: PeerFeature])).toMap

  private val handshakeSerializer = new HandshakeSpec(featureSerializers, settings.maxHandshakeSize)
  private val messageSerializer = new MessageSerializer(bifrostContext.messageSpecs, settings.magicBytes)

  // there is no recovery for broken connections
  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private var selfPeer: Option[ConnectedPeer] = None

  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None

  private var chunksBuffer: ByteString = CompactByteString.empty

  private var outMessagesBuffer: TreeMap[Long, ByteString] = TreeMap.empty

  private var outMessagesCounter: Long = 0

  override def preStart: Unit = {
    context watch connection
    connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)
    connection ! ResumeReading

    context.become(handshaking)
  }

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXTS
  // The following functions are the contexts that a peerConnectionHandler can occupy
  // depending on the state of the connection.

  // we don't expect the default receive to be used due to prestart context change
  override def receive: Receive = nonsense

  private def handshaking: Receive = {
    handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.handshakeTimeout)
    (self ! HandshakeTimeout))
    val hb = handshakeSerializer.toBytes(createHandshakeMessage())
    connection ! Tcp.Write(ByteString(hb))
    log.info(s"Handshake sent to $connectionId")

    // note: JAA made a change here that might break handshaking 2020.06.04 (remove if still here in 3 months)
    receiveAndHandleHandshake orElse
    handshakeTimeout orElse
    fatalCommands
  }

  private def workingCycleWriting: Receive =
    localInterfaceWriting orElse
    remoteInterface orElse
    fatalCommands orElse
    nonsense

  private def workingCycleBuffering: Receive =
    localInterfaceBuffering orElse
    remoteInterface orElse
    fatalCommands orElse
    nonsense

  private def closingWithNonEmptyBuffer: Receive =
    closeNonEmptyBuffer orElse
    nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS
  // The following functions are used to process the incoming messages based on the current context.
  // Some functions are only accessible in certain contexts so be sure to consult the lists above

  private def receiveAndHandleHandshake: Receive = {
    case Received(data) =>
      handshakeSerializer.parseBytesTry(data.toArray) match {
        case Success(handshake) =>
          processHandshake(handshake)

        case Failure(t) =>
          log.info(s"Error during parsing a handshake", t)
          //ban the peer for the wrong handshake message
          //peer will be added to the blacklist and the network controller will send CloseConnection
          selfPeer.foreach(c => networkControllerRef ! PenalizePeer(c.connectionId.remoteAddress, PenaltyType.PermanentPenalty))
      }
  }

  private def handshakeTimeout: Receive = {
    case HandshakeTimeout =>
      log.info(s"Handshake timeout with $connectionId, going to drop the connection")
      self ! CloseConnection
  }

  private def localInterfaceWriting: Receive = {
    case msg: message.Message[_] =>
      log.info("Send message " + msg.spec + " to " + connectionId)
      outMessagesCounter += 1
      connection ! Write(messageSerializer.serialize(msg), ReceivableMessages.Ack(outMessagesCounter))

    case CommandFailed(Write(msg, ReceivableMessages.Ack(id))) =>
      log.warn(s"Failed to write ${msg.length} bytes to $connectionId, switching to buffering mode")
      connection ! ResumeWriting
      buffer(id, msg)
      context become workingCycleBuffering

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + connectionId + ", switching to closing mode")
      if (outMessagesBuffer.isEmpty) connection ! Close else context become closingWithNonEmptyBuffer

    case ReceivableMessages.Ack(_) => // ignore ACKs in stable mode

    case WritingResumed => // ignore in stable mode
  }

  // operate in ACK mode until all buffered messages are transmitted
  private def localInterfaceBuffering: Receive = {
    case msg: message.Message[_] =>
      outMessagesCounter += 1
      buffer(outMessagesCounter, messageSerializer.serialize(msg))

    case CommandFailed(Write(msg, ReceivableMessages.Ack(id))) =>
      connection ! ResumeWriting
      buffer(id, msg)

    case CommandFailed(ResumeWriting) => // ignore in ACK mode

    case WritingResumed =>
      writeFirst()

    case ReceivableMessages.Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.nonEmpty) writeFirst()
      else {
        log.info("Buffered messages processed, exiting buffering mode")
        context become workingCycleWriting
      }

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + connectionId + s", switching to closing mode")
      writeAll()
      context become closingWithNonEmptyBuffer
  }

  private def closeNonEmptyBuffer: Receive = {
    case CommandFailed(_: Write) =>
      connection ! ResumeWriting
      context.become({
        case WritingResumed =>
          writeAll()
          context.unbecome()
        case ReceivableMessages.Ack(id) =>
          outMessagesBuffer -= id
      }, discardOld = false)

    case ReceivableMessages.Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.isEmpty) connection ! Close
  }

  private def remoteInterface: Receive = {
    case Received(data) =>
      chunksBuffer ++= data
      processRemoteData()
      connection ! ResumeReading
  }

  private def fatalCommands: Receive = {
    case _: ConnectionClosed =>
      log.info(s"Connection closed to $connectionId")
      context stop self
  }

  private def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"PeerConnectionHandler (in context ${context.toString}): got unexpected input $nonsense")
  }

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  override def postStop(): Unit = log.info(s"Peer handler to $connectionId destroyed")

  private def buffer(id: Long, msg: ByteString): Unit = {
    outMessagesBuffer += id -> msg
  }

  private def writeFirst(): Unit = {
    outMessagesBuffer.headOption.foreach { case (id, msg) =>
      connection ! Write(msg, ReceivableMessages.Ack(id))
    }
  }

  private def writeAll(): Unit = {
    outMessagesBuffer.foreach { case (id, msg) =>
      connection ! Write(msg, ReceivableMessages.Ack(id))
    }
  }

  private def createHandshakeMessage() = {
    Handshake(
      PeerSpec(
        settings.agentName,
        Version(settings.appVersion),
        settings.nodeName,
        ownSocketAddress,
        localFeatures
      ),
      bifrostContext.timeProvider.time()
    )
  }

  private def processHandshake(receivedHandshake: Handshake): Unit = {
    log.info(s"Got a Handshake from $connectionId")

    val peerInfo = PeerInfo(
      receivedHandshake.peerSpec,
      bifrostContext.timeProvider.time(),
      Some(direction)
    )
    val peer = ConnectedPeer(connectionDescription.connectionId, self, Some(peerInfo))
    selfPeer = Some(peer)

    networkControllerRef ! Handshaked(peerInfo)
    handshakeTimeoutCancellableOpt.map(_.cancel())
    connection ! ResumeReading
    context become workingCycleWriting
  }

  @tailrec
  private def processRemoteData(): Unit = {
    messageSerializer.deserialize(chunksBuffer, selfPeer) match {
      case Success(Some(message)) =>
        log.info("Received message " + message.spec + " from " + connectionId)
        networkControllerRef ! message
        chunksBuffer = chunksBuffer.drop(message.messageLength)
        processRemoteData()
      case Success(None) =>
      case Failure(e) =>
        e match {
          //peer is doing bad things, ban it
          case MaliciousBehaviorException(msg) =>
            log.warn(s"Banning peer for malicious behaviour($msg): ${connectionId.toString}")
            //peer will be added to the blacklist and the network controller will send CloseConnection
            networkControllerRef ! PenalizePeer(connectionId.remoteAddress, PenaltyType.PermanentPenalty)
          //non-malicious corruptions
          case _ =>
            log.info(s"Corrupted data from ${connectionId.toString}: ${e.getMessage}")
        }
    }
  }

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object PeerConnectionHandler {

  object ReceivableMessages {

    private[PeerConnectionHandler] object HandshakeDone

    case object StartInteraction

    case object HandshakeTimeout

    case object CloseConnection

    final case class Ack(id: Long) extends Tcp.Event

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object PeerConnectionHandlerRef {
  def props(settings: NetworkSettings,
            networkControllerRef: ActorRef,
            bifrostContext: BifrostContext,
            connectionDescription: ConnectionDescription
           )(implicit ec: ExecutionContext): Props =
    Props(new PeerConnectionHandler(settings, networkControllerRef, bifrostContext, connectionDescription))

  def apply(settings: NetworkSettings,
            networkControllerRef: ActorRef,
            bifrostContext: BifrostContext,
            connectionDescription: ConnectionDescription)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, networkControllerRef, bifrostContext, connectionDescription))

  def apply(name: String,
            settings: NetworkSettings,
            networkControllerRef: ActorRef,
            bifrostContext: BifrostContext,
            connectionDescription: ConnectionDescription)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(settings, networkControllerRef, bifrostContext, connectionDescription), name)
}
