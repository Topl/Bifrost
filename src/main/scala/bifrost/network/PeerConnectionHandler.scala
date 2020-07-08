package bifrost.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Cancellable, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import bifrost.network.message.MessageHandler
import bifrost.network.peer.PeerManager
import bifrost.network.peer.PeerManager.{AddToBlacklist, Handshaked}
import bifrost.settings.Settings
import bifrost.utils.Logging
import com.google.common.primitives.Ints

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

case class ConnectedPeer(socketAddress: InetSocketAddress, handlerRef: ActorRef) {

  import shapeless.syntax.typeable._

  override def equals(obj: scala.Any): Boolean =
    obj.cast[ConnectedPeer].exists(_.socketAddress.getAddress.getHostAddress == this.socketAddress.getAddress.getHostAddress)
}


case object Ack extends Event


case class PeerConnectionHandler(settings: Settings,
                                 networkControllerRef: ActorRef,
                                 peerManager: ActorRef,
                                 messagesHandler: MessageHandler,
                                 connection: ActorRef,
                                 ownSocketAddress: Option[InetSocketAddress],
                                 remote: InetSocketAddress) extends Actor with Buffering with Logging {

  import PeerConnectionHandler._

  private val selfPeer = ConnectedPeer(remote, self)

  context watch connection

  override def preStart: Unit = connection ! ResumeReading

  // there is no recovery for broken connections
  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  private def processErrors(stateName: String): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"Write failed :$w " + remote + s" in state $stateName")
      //      peerManager ! AddToBlacklist(remote)
      connection ! Close
      connection ! ResumeReading
      connection ! ResumeWriting

    case cc: ConnectionClosed =>
      peerManager ! PeerManager.Disconnected(remote)
      log.debug("Connection closed to : " + remote + ": " + cc.getErrorCause + s" in state $stateName")
      context stop self

    case CloseConnection =>
      log.debug(s"Enforced to abort communication with: " + remote + s" in state $stateName")
      connection ! Close

    case CommandFailed(cmd: Tcp.Command) =>
      log.debug("Failed to execute command : " + cmd + s" in state $stateName")
      connection ! ResumeReading
  }

  private var handshakeGot = false
  private var handshakeSent = false

  private var handshakeTimeoutCancellableOpt: Option[Cancellable] = None

  private object HandshakeDone

  private def handshake: Receive = ({
    case StartInteraction =>
      val hb = Handshake(
        settings.agentName,
        settings.appVersion,
        settings.nodeName,
        settings.nodeNonce,
        ownSocketAddress,
        System.currentTimeMillis()
      ).bytes

      connection ! Write(ByteString(hb))
      log.debug(s"Handshake sent to $remote")
      handshakeSent = true
      if (handshakeGot && handshakeSent){
        self ! HandshakeDone
      } else {
        handshakeTimeoutCancellableOpt = Some(context.system.scheduler.scheduleOnce(settings.handshakeTimeout.millis)(self ! HandshakeTimeout))
      }

    case Received(data) =>
      HandshakeSerializer.parseBytes(data.toArray) match {
        case Success(handshake) =>
          peerManager ! Handshaked(remote, handshake)
          log.debug(s"Got a Handshake from $remote")
          connection ! ResumeReading
          handshakeGot = true
          if (handshakeGot && handshakeSent) self ! HandshakeDone
        case Failure(t) =>
          log.debug(s"Error during parsing a handshake", t)
          //todo: blacklist?
          connection ! Close
      }

    case HandshakeTimeout =>
      connection ! Close

    case HandshakeDone =>
      handshakeTimeoutCancellableOpt.map(_.cancel())
      connection ! ResumeReading
      context become workingCycle
  }: Receive) orElse processErrors(CommunicationState.AwaitingHandshake.toString)


  def workingCycleLocalInterface: Receive = {
    case msg: message.Message[_] =>
      def sendOutMessage() {
        val bytes = msg.bytes
        log.debug("Send message " + msg.spec + " to " + remote)
        connection ! Write(ByteString(Ints.toByteArray(bytes.length) ++ bytes))
      }

      //simulating network delays
      settings.addedMaxDelay match {
        case Some(delay) =>
          context.system.scheduler.scheduleOnce(Random.nextInt(delay).millis)(sendOutMessage())
        case None =>
          sendOutMessage()
      }

    case Blacklist =>
      log.debug(s"Going to blacklist " + remote)
      peerManager ! AddToBlacklist(remote)
      connection ! Close
  }

  private var chunksBuffer: ByteString = CompactByteString()

  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>

      val t = getPacket(chunksBuffer ++ data)
      chunksBuffer = t._2

      t._1.find { packet =>
        messagesHandler.parseBytes(packet.toByteBuffer, Some(selfPeer)) match {
          case Success(message) =>
            log.debug("Received message " + message.spec + " from " + remote)
            networkControllerRef ! message
            false

          case Failure(e) =>
            log.debug(s"Corrupted data from: " + remote, e)
            //  connection ! Close
            //  context stop self
            true
        }
      }
      connection ! ResumeReading
  }

  def workingCycle: Receive =
    workingCycleLocalInterface orElse
      workingCycleRemoteInterface orElse
      processErrors(CommunicationState.WorkingCycle.toString) orElse ({
      case nonsense: Any =>
        log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
    }: Receive)

  override def receive: Receive = handshake
}

object PeerConnectionHandler {
  case object StartInteraction

  private object CommunicationState extends Enumeration {
    type CommunicationState = Value

    val AwaitingHandshake = Value("AwaitingHandshake")
    val WorkingCycle = Value("WorkingCycle")
  }

  case object HandshakeTimeout

  case object CloseConnection

  case object Blacklist
}
