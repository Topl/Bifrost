package bifrost.network

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpec
import scorex.crypto.hash.Keccak256
import serializer.PeerMessage

import scala.util.Try

object PeerMessageSpec extends MessageSpec[PeerMessage] {

  override val messageCode: MessageCode = Keccak256("PeerMessage").head: Byte
  override val messageName: String = "PeerMessage"

  override def parseBytes(bytes: Array[Byte]): Try[PeerMessage] = Try {
    serializer.PeerMessage.parseFrom(bytes)
  }

  override def toBytes(data: PeerMessage): Array[Byte] = serializer.PeerMessage.toByteArray(data)
}