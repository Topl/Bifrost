package co.topl.network.message

import co.topl.codecs._
import co.topl.crypto.hash.blake2b256
import co.topl.nodeCodecs.binary.network._

case class TransmissionHeader(code: MessageCode, dataLength: Int)

case class TransmissionContent(checksum: Array[Byte], data: Array[Byte])

case class Transmission(header: TransmissionHeader, content: Option[TransmissionContent]) {

  def decodeMessage: Either[String, Message] =
    (header.code +: content.map(_.data).getOrElse(Array.emptyByteArray)).decodeTransmitted[Message]
}

object Transmission {
  val magicLength: Int = 4
  val checksumLength: Int = 4

  // header is MAGIC ++ Array(spec.messageCode) ++ Ints.toByteArray(dataLength) ++ dataWithChecksum */
  val headerLength: Int = magicLength + 1 + 4 + checksumLength

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
