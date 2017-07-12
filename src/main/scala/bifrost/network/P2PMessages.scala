package bifrost.network

import scorex.core.network.message.Message.MessageCode
import scorex.core.network.message.MessageSpec

import scala.util.Try

class ProducerNotifySpec extends MessageSpec[ProducerProposal] {

  override val messageCode: MessageCode = 1: Byte
  override val messageName: String = "ProducerProposal"

  override def parseBytes(bytes: Array[Byte]): Try[ProducerProposal] = Try {
    new ProducerProposal()
  }

  override def toBytes(data: ProducerProposal): Array[Byte] = Array[Byte]()
}

class ProducerProposal {

}