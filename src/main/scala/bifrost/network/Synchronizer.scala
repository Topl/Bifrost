package bifrost.network

import akka.actor.ActorRef
import bifrost.network.NetworkController.ReceivableMessages.PenalizePeer
import bifrost.network.message.{MessageSpec, Message}
import bifrost.network.peer.{ConnectedPeer, PenaltyType}
import bifrost.utils.Logging

import scala.util.{Failure, Success}

trait Synchronizer extends Logging {

  val networkControllerRef: ActorRef
  protected val msgHandlers: PartialFunction[Message[_], Unit] // these are the case statements for identifying the message handlers

  protected def parseAndHandle(spec: MessageSpec[Any], msgBytes: Array[Byte], source: Option[ConnectedPeer]): Unit = {
    // attempt to parse the message
    spec.parseBytes(msgBytes) match {
      // if a message could be parsed, match the type of content found and ensure a handler is defined
      case Success(content) =>
        val msgWithContent = Message(spec, Right(content), source)
        if (msgHandlers.isDefinedAt(msgWithContent)) msgHandlers.apply(msgWithContent)
        else log.error(s"Function handler not found for the parsed message: $msgWithContent")

      // if a message could not be parsed, penalize the remote peer
      case Failure(e) =>
        log.error(s"Failed to deserialize data from ${source.getOrElse("UNKNOWN")}: ", e)
        networkControllerRef ! PenalizePeer(
          source.get.connectionId.remoteAddress,
          PenaltyType.PermanentPenalty
        )
  }
}

}
