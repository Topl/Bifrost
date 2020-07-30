package bifrost.modifier.box

import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import com.google.common.primitives.Ints
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

case class AssetBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    amount: Long,
                    assetCode: String,
                    issuer: PublicKey25519Proposition,
                    data: String) extends NoncedBox(proposition, nonce, amount) {
  override lazy val typeOfBox: String = "Asset"

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "assetCode" -> assetCode.asJson,
    "value" -> value.toString.asJson,
    "issuer" -> Base58.encode(issuer.pubKeyBytes).asJson,
    "data" -> data.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson
}

object AssetBoxSerializer extends BifrostSerializer[AssetBox] with NoncedBoxSerializer {

  override def toBytes(obj: AssetBox): Array[Byte] = {
    noncedBoxToBytes(obj, "AssetBox") ++
      obj.issuer.pubKeyBytes ++
      obj.assetCode.getBytes ++
      Ints.toByteArray(obj.assetCode.getBytes.length) ++
      obj.data.getBytes ++
      Ints.toByteArray(obj.data.getBytes.length)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AssetBox] = Try {

    val params = noncedBoxParseBytes(bytes)

    val dataLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))

    val assetLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES - Ints.BYTES - dataLen,
      bytes.length - Ints.BYTES - dataLen))
    val asset: String = new String(bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen,
      bytes.length - (2 * Ints.BYTES) - dataLen))

    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(
      bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen - Constants25519.PubKeyLength,
        bytes.length - (2 * Ints.BYTES) - dataLen - assetLen)
    )

    AssetBox(params._1, params._2, params._3, asset, issuer, data)
  }

  override def serialize(obj: AssetBox, w: Writer): Unit = ???

  override def parse(r: Reader): AssetBox = ???
}
