package co.topl.network

import akka.actor.Actor
import co.topl.network.message.{Message, MessageCode, Transmission}
import co.topl.network.peer.ConnectedPeer
import co.topl.settings.Version
import co.topl.utils.Logging

trait Synchronizer extends Actor with Logging {

  /** These are the case statements for identifying the message handlers */
  protected val msgHandlers: PartialFunction[(Message, ConnectedPeer), Unit]

  /** Receive data from peer and start parsing and handling the message depending on the message spec */
  protected def processDataFromPeer: Receive = { case Synchronizer.TransmissionReceived(transmission, source) =>
    parseAndHandle(transmission, source)
  }

  /**
   * This method will attempt to parse a message from a remote peer into it class representation and use
   * the defined message handlers for processing the message
   *
   * @param spec the message specification (basically a header informing of the message type)
   * @param msgBytes a ByteString of the message data that must be parsed
   * @param source the remote peer that sent the message
   */
  protected def parseAndHandle(
    transmission: Transmission,
    source:       ConnectedPeer
  ): Unit =
    transmission.decodeMessage match {

      case Right(message) if msgHandlers.isDefinedAt((message, source)) =>
        msgHandlers.apply((message, source))

      case Right(message) =>
        log.error(s"Function handler not found for the provided message: $message")

      case Left(decodingError) =>
        log.error(s"Failed to deserialize data from $source: $decodingError")
        penalizeMaliciousPeer(source)

    }

  /**
   * Handles how a peer that sent un-parsable data should be handled
   *
   * @param peer peer that sent the offending message
   */
  protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit
}

object Synchronizer {

  final case class TransmissionReceived(
    transmission: Transmission,
    peer:         ConnectedPeer
  )
}
