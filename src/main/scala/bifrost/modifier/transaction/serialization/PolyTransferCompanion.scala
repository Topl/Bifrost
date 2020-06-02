package bifrost.modifier.transaction.serialization

import bifrost.modifier.transaction.bifrostTransaction.PolyTransfer
import bifrost.utils.serialization.BifrostSerializer
import com.google.common.primitives.Ints

import scala.util.Try

object PolyTransferCompanion extends BifrostSerializer[PolyTransfer] with TransferSerializer {

  override def toBytes(sc: PolyTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(sc)
  }

  def toChildBytes(sc: PolyTransfer): Array[Byte] = {
    transferToBytes(sc, "PolyTransfer") ++
    sc.data.getBytes ++
    Ints.toByteArray(sc.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[PolyTransfer] = Try {
    val params = parametersParseBytes(bytes)
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )
    PolyTransfer(params._1, params._2, params._3, params._4, params._5, data)
  }
}
