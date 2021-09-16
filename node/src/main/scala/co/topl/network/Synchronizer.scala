package co.topl.network

import akka.actor.Actor
import co.topl.network.message.{Message, MessageSpec}
import co.topl.network.peer.ConnectedPeer
import co.topl.utils.Logging

import scala.util.{Failure, Success}

trait Synchronizer extends Actor with Logging {

  /** These are the case statements for identifying the message handlers */
  protected val msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit]

  /** Receive data from peer and start parsing and handling the message depending on the message spec */
  protected def processDataFromPeer: Receive = { case Message(spec, Left(msgBytes), Some(source)) =>
    parseAndHandle(spec, msgBytes, source)
  }

  /**
   * This method will attempt to parse a message from a remote peer into it class representation and use
   * the defined message handlers for processing the message
   *
   * @param spec the message specification (basically a header informing of the message type)
   * @param msgBytes a ByteString of the message data that must be parsed
   * @param source the remote peer that sent the message
   */
  protected def parseAndHandle(spec: MessageSpec[Any], msgBytes: Array[Byte], source: ConnectedPeer): Unit =
    /** Attempt to parse the message */
    spec.parseBytes(msgBytes) match {
      /** If a message could be parsed, match the type of content found and ensure a handler is defined */
      case Success(content) =>
        val parsedMsg = (spec, content, source)
        if (msgHandlers.isDefinedAt(parsedMsg)) msgHandlers.apply(parsedMsg)
        else log.error(s"Function handler not found for the parsed message: $parsedMsg")

      /** If a message could not be parsed, penalize the remote peer */
      case Failure(e) =>
        log.error(s"Failed to deserialize data from $source: ", e)
        penalizeMaliciousPeer(source)
    }

  /**
   * Handles how a peer that sent un-parsable data should be handled
   *
   * @param peer peer that sent the offending message
   */
  protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit
}
