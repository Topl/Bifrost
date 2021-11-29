package co.topl.network.message

import co.topl.codecs._
import co.topl.crypto.hash.blake2b256
import co.topl.nodeCodecs.binary.network._

/**
 * The header info of the transmitted message.
 * @param code the message code identifying the message data
 * @param dataLength the length of the message data
 */
case class TransmissionHeader(code: MessageCode, dataLength: Int)

/**
 * @param checksum the data checksum, should be the first 4 bytes of the Blake-2b256 hash of the message data
 * @param data the encoded data of the message
 */
case class TransmissionContent(checksum: Array[Byte], data: Array[Byte])

/**
 * Wrapper for a transmitted network message, whether it came from an external peer or was generated locally.
 *
 * @param header the header of the transmitted message
 * @param content the message's checksum and encoded content
 */
case class Transmission(header: TransmissionHeader, content: Option[TransmissionContent]) {

  def decodeMessage: Either[String, Message] =
    (header.code +: content.map(_.data).getOrElse(Array.emptyByteArray)).decodeTransmitted[Message]
}

object Transmission {
  val magicLength: Int = 4
  val checksumLength: Int = 4

  // header is MAGIC ++ message code ++ data length */
  val headerLength: Int = magicLength + 1 + 4

  def checksum(messageData: Array[Byte]): Array[Byte] =
    blake2b256.hash(messageData).value.take(Transmission.checksumLength)

  def encodeMessage(message: Message): Transmission = {
    val messageData = message.transmittableBytes.tail

    Transmission(
      TransmissionHeader(message.messageCode, messageData.length),
      Option.when(messageData.length > 0)(TransmissionContent(checksum(messageData), messageData))
    )
  }
}
