package co.topl.network

import cats.implicits._
import akka.actor.{Actor, ActorRef, Cancellable, Props, SupervisorStrategy}
import akka.io.Tcp
import akka.util.{ByteString, CompactByteString}
import co.topl.codecs._
import co.topl.network.NetworkController.ReceivableMessages.{Handshaked, PenalizePeer}
import co.topl.network.PeerConnectionHandler.ReceivableMessages._
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Message, Transmission, TransmissionHeader}
import co.topl.network.peer.PenaltyType.PermanentPenalty
import co.topl.network.peer._
import co.topl.settings.{AppContext, AppSettings}
import co.topl.utils.{Logging, TimeProvider}
import co.topl.codecs._
import co.topl.codecs.binary.typeclasses.Transmittable
import co.topl.nodeCodecs.binary.network._
import scodec.bits.BitVector

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

class PeerConnectionHandler(
  networkControllerRef:  ActorRef,
  settings:              AppSettings,
  appContext:            AppContext,
  connectionDescription: ConnectionDescription
)(implicit timeProvider: TimeProvider)
    extends Actor
    with Logging {

  import context.dispatcher

  private val connection = connectionDescription.connection
  private val connectionId = connectionDescription.connectionId
  private val direction = connectionDescription.connectionId.direction
  private val ownSocketAddress = connectionDescription.ownSocketAddress
  private val localFeatures = connectionDescription.localFeatures

  private val localPeerSpec = PeerSpec(
    settings.network.agentName,
    settings.application.version,
    settings.network.nodeName,
    ownSocketAddress,
    localFeatures
  )

  /** there is no recovery for broken connections */
  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private var selfPeer: Option[ConnectedPeer] = None

  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None

  private var chunksBuffer: ByteString = CompactByteString.empty

  private var outMessagesBuffer: TreeMap[Long, ByteString] = TreeMap.empty

  private var outMessagesCounter: Long = 0

  // transmittable type-class instances for providing extension methods for converting to/from transmitted data
  implicit private val transmissionHeaderTransmittableInstance: Transmittable[TransmissionHeader] =
    transmissionHeaderTransmittable(settings.network.magicBytes)

  implicit private val transmissionTransmittableInstance: Transmittable[Transmission] =
    transmissionTransmittable(settings.network.magicBytes)

  override def preStart(): Unit = {

    /** per Akka docs this "signs the death pact: this actor terminates when connection breaks" */
    context watch connection
    connection ! Tcp.Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)

    /**
     * On instantiation of a PeerConnectionHandler, send ResumeReading to the TCP system since the
     * connection was created with pullMode = true (this is done in the NetworkController)
     * https://doc.akka.io/docs/akka/current/io-tcp.html#read-back-pressure-with-pull-mode
     */
    connection ! Tcp.ResumeReading

    /** transition to create and listen for a handshake from the remote peer */
    context become handshaking
  }

  override def postStop(): Unit = log.info(s"Peer handler to $connectionId destroyed")

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXTS ----------- //
  /** The following functions are the contexts that a peerConnectionHandler can occupy */
  /** depending on the state of the connection. */

  /** we don't expect the default receive to be used due to prestart context change */
  override def receive: Receive = nonsense

  private def handshaking: Receive = {

    /** When the PCH context becomes handshaking, this will create a handshake that is sent to the remote peer */
    /** NOTE: This process is only executed once and subsequent messages are handled by the partial functions below */
    createHandshakeMessage() // create handshake with timeout and then continue to process messages

    /** receive and processes message from remote peer (prior to sending data) */
    receiveAndHandleHandshake orElse
    handshakeTimeout orElse
    fatalCommands orElse
    nonsense
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

  // ----------- MESSAGE PROCESSING FUNCTIONS ----------- //
  /** The following functions are used to process the incoming messages based on the current context */
  /** Some functions are only accessible in certain contexts so be sure to consult the lists above */

  private def receiveAndHandleHandshake: Receive = { case Tcp.Received(data) =>
    data.toArray.decodeTransmitted[MessagesV1.Handshake] match {
      case Right(handshake) =>
        processHandshake(handshake)

      case Left(err) =>
        log.info(s"Error during parsing a handshake", err)

        /** ban the peer for the wrong handshake message */
        /** peer will be added to the blacklist and the network controller will send CloseConnection */
        selfPeer.foreach(c => networkControllerRef ! PenalizePeer(c.connectionId.remoteAddress, PermanentPenalty))
    }
  }

  private def handshakeTimeout: Receive = { case HandshakeTimeout =>
    log.info(s"Handshake timeout with $connectionId, going to drop the connection")
    connection ! Tcp.Close
  }

  private def localInterfaceWriting: Receive = {
    case transmission: Transmission =>
      log.info("Send message " + transmission.header.code + " to " + connectionId)
      outMessagesCounter += 1
      connection ! Tcp.Write(ByteString(transmission.transmittableBytes), Ack(outMessagesCounter))

    case Tcp.CommandFailed(Tcp.Write(msg, Ack(id))) =>
      log.warn(s"Failed to write ${msg.length} bytes to $connectionId, switching to buffering mode")
      connection ! Tcp.ResumeWriting
      buffer(id, msg)
      context become workingCycleBuffering

    /** ignore in stable mode */
    case Tcp.WritingResumed =>

    /** ignore ACKs in stable mode */
    case Ack(_) =>

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + connectionId + ", switching to closing mode")
      if (outMessagesBuffer.isEmpty) connection ! Tcp.Close else context become closingWithNonEmptyBuffer
  }

  /** operate in ACK mode until all buffered messages are transmitted */
  private def localInterfaceBuffering: Receive = {
    case transmission: Transmission =>
      outMessagesCounter += 1
      buffer(outMessagesCounter, transmission.transmittableByteString)

    case Tcp.CommandFailed(Tcp.Write(msg, Ack(id))) =>
      connection ! Tcp.ResumeWriting
      buffer(id, msg)

    /** ignore in ACK mode */
    case Tcp.CommandFailed(Tcp.ResumeWriting) =>

    case Tcp.WritingResumed =>
      writeFirst()

    case Ack(id) =>
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
    case Tcp.CommandFailed(_: Tcp.Write) =>
      connection ! Tcp.ResumeWriting
      context.become(
        {
          case Tcp.WritingResumed =>
            writeAll()
            context.unbecome()
          case Ack(id) =>
            outMessagesBuffer -= id
        },
        discardOld = false
      )

    case Ack(id) =>
      outMessagesBuffer -= id
      if (outMessagesBuffer.isEmpty) connection ! Tcp.Close
  }

  private def remoteInterface: Receive = { case Tcp.Received(data) =>
    chunksBuffer ++= data
    processRemoteData()
    connection ! Tcp.ResumeReading
  }

  private def fatalCommands: Receive = { case _: Tcp.ConnectionClosed =>
    log.info(s"Connection closed to $connectionId")
    context stop self
  }

  private def nonsense: Receive = { case nonsense: Any =>
    log.warn(s"PeerConnectionHandler (in context ${context.toString}): got unexpected input $nonsense from ${sender()}")
  }

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  private def buffer(id: Long, msg: ByteString): Unit =
    outMessagesBuffer += id -> msg

  private def writeFirst(): Unit =
    outMessagesBuffer.headOption.foreach { case (id, msg) =>
      connection ! Tcp.Write(msg, Ack(id))
    }

  private def writeAll(): Unit =
    outMessagesBuffer.foreach { case (id, msg) =>
      connection ! Tcp.Write(msg, Ack(id))
    }

  private def createHandshakeMessage(): Unit = {
    val nodeInfo = MessagesV1.Handshake(localPeerSpec, timeProvider.time)

    /**
     * create, save, and schedule a timeout option. The variable lets us cancel the timeout message
     * if a handshake is received
     */
    handshakeTimeoutCancellableOpt = Some(
      context.system.scheduler.scheduleOnce(settings.network.handshakeTimeout)(self ! HandshakeTimeout)
    )

    /** send a handshake message with our node information to the remote peer */
    connection ! Tcp.Write(ByteString(nodeInfo.transmittableBytes), Tcp.NoAck)
    log.info(s"Handshake sent to $connectionId")
  }

  private def processHandshake(receivedHandshake: MessagesV1.Handshake): Unit = {
    log.info(s"Got a Handshake from $connectionId")

    val peerInfo = PeerInfo(
      receivedHandshake.peerSpec,
      timeProvider.time,
      Some(direction)
    )
    val peer = ConnectedPeer(connectionDescription.connectionId, self, Some(peerInfo))
    selfPeer = Some(peer)

    networkControllerRef ! Handshaked(peerInfo)
    handshakeTimeoutCancellableOpt.map(_.cancel())
    connection ! Tcp.ResumeReading
    context become workingCycleWriting
  }

  @tailrec
  private def processRemoteData(): Unit =
    if (chunksBuffer.length >= Transmission.headerLength) {
      chunksBuffer.decodeTransmitted[TransmissionHeader] match {
        case Right(header) if header.dataLength == 0 =>
          handleTransmission(Transmission(header, None))
          processRemoteData()
        case Right(header)
            if chunksBuffer.length < Transmission.headerLength + Transmission.checksumLength + header.dataLength =>
        // need more data
        case Right(header) =>
          transmissionContentTransmittable(header.dataLength)
            .fromTransmittableByteString(chunksBuffer.drop(Transmission.headerLength)) match {
            case Right(content) =>
              handleTransmission(Transmission(header, content.some))
              processRemoteData()
            case Left(error) =>
              log.warn(s"Banning peer for malicious behaviour($error): ${connectionId.toString}")

              /** peer will be added to the blacklist and the network controller will send CloseConnection */
              networkControllerRef ! PenalizePeer(connectionId.remoteAddress, PenaltyType.PermanentPenalty)
          }
        case Left(error) =>
          log.warn(s"Banning peer for malicious behaviour($error): ${connectionId.toString}")

          /** peer will be added to the blacklist and the network controller will send CloseConnection */
          networkControllerRef ! PenalizePeer(connectionId.remoteAddress, PenaltyType.PermanentPenalty)
      }
    }

  /**
   * Handles processing an incoming transmission and updating the state of the chunks buffer.
   * @param transmission the transmission that is ready for processing
   */
  private def handleTransmission(transmission: Transmission): Unit = {
    log.info("Received message " + transmission.header.code + " from " + connectionId)

    networkControllerRef ! NetworkController.ReceivableMessages.TransmissionReceived(transmission, selfPeer)

    chunksBuffer = chunksBuffer.drop(
      Transmission.headerLength +
      transmission.content.map(content => Transmission.checksumLength + content.data.length).getOrElse(0)
    )
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

  def props(
    networkControllerRef:  ActorRef,
    settings:              AppSettings,
    appContext:            AppContext,
    connectionDescription: ConnectionDescription
  )(implicit timeProvider: TimeProvider): Props =
    Props(new PeerConnectionHandler(networkControllerRef, settings, appContext, connectionDescription))
}
