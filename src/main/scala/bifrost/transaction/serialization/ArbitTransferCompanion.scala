package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.ArbitTransfer
import com.google.common.primitives.Ints

import scala.util.Try

object ArbitTransferCompanion extends Serializer[ArbitTransfer] with TransferSerializer {

  override def toBytes(ac: ArbitTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(ac)
  }

  def toChildBytes(ac: ArbitTransfer): Array[Byte] = {
    transferToBytes(ac, "ArbitTransfer") ++
    ac.data.getBytes++
    Ints.toByteArray(ac.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[ArbitTransfer] = Try {
    val params = parametersParseBytes(bytes)
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )
    ArbitTransfer(params._1, params._2, params._3, params._4, params._5, data)
  }
}
