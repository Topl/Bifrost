package bifrost.network

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpec
import scorex.crypto.hash.Keccak256
import serializer.ProducerProposal

import scala.util.Try

object ProducerNotifySpec extends MessageSpec[ProducerProposal] {

  override val messageCode: MessageCode = Keccak256("ProducerProposal").head: Byte
  override val messageName: String = "ProducerProposal"

  override def parseBytes(bytes: Array[Byte]): Try[ProducerProposal] = Try {
    serializer.ProducerProposal.parseFrom(bytes)
  }

  override def toBytes(data: ProducerProposal): Array[Byte] = serializer.ProducerProposal.toByteArray(data)
}