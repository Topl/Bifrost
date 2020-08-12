package bifrost.modifier.transaction.serialization

import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.modifier.transaction.bifrostTransaction.AssetTransfer
import bifrost.serialization.Serializer
import com.google.common.primitives.Ints

import scala.util.Try

object AssetTransferCompanion extends Serializer[AssetTransfer] with TransferSerializer {

  override def toBytes(at: AssetTransfer): Array[Byte] = {
    TransferTransactionCompanion.prefixBytes ++ toChildBytes(at)
  }

  def toChildBytes(at: AssetTransfer): Array[Byte] = {
    transferToBytes(at, "AssetTransfer") ++
      at.issuer.pubKeyBytes ++
      at.assetCode.getBytes ++
      Ints.toByteArray(at.assetCode.getBytes.length)++
      at.data.getBytes++
      Ints.toByteArray(at.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetTransfer] = Try {
    val params = parametersParseBytes(bytes)

    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )

    val assetCodeLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES - dataLen - Ints.BYTES, bytes.length - Ints.BYTES - dataLen))
    val assetCode: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen, bytes.length - Ints.BYTES - dataLen - Ints.BYTES)
    )

    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen - Constants25519.PubKeyLength,
        bytes.length - Ints.BYTES - assetCodeLen - Ints.BYTES - dataLen)
    )

    AssetTransfer(params._1, params._2, params._3, issuer, assetCode, params._4, params._5, data)
  }
}
