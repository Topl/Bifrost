package bifrost.modifier.box.serialization

import bifrost.modifier.box.proposition.{Constants25519, PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.modifier.box.{AssetBox, NoncedBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object AssetBoxSerializer extends BifrostSerializer[AssetBox] {

  override def serialize(obj: AssetBox, w: Writer): Unit = {

    // TODO: Jing - Do we want to serialize in the same order as the fields in the case class? (asset-issuer-data)
    // TODO: Jing - Do we want to stick with the same naming scheme? (amount/value)
    NoncedBoxSerializer.serialize(obj, w)

    /* assetCode: String */
    w.putIntString(obj.assetCode)

    /* issuer: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.issuer, w)

    /* data: String */
    w.putIntString(obj.data)
  }

  override def parse(r: Reader): AssetBox = {
    val noncedBox: NoncedBox = NoncedBoxSerializer.parse(r)

    // putIntString encode String that is shorter than 2147483647 bytes
    val asset: String = r.getIntString()

    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(r.getBytes(Constants25519.PubKeyLength))
    val data: String = r.getIntString()

    AssetBox(noncedBox.proposition, noncedBox.nonce, noncedBox.value, asset, issuer, data)
  }

  // TODO: Jing - remove
  //
  //  override def toBytes(obj: AssetBox): Array[Byte] = {
  //    noncedBoxToBytes(obj, "AssetBox") ++
  //      obj.issuer.pubKeyBytes ++
  //      obj.assetCode.getBytes ++
  //      Ints.toByteArray(obj.assetCode.getBytes.length) ++
  //      obj.data.getBytes ++
  //      Ints.toByteArray(obj.data.getBytes.length)
  //  }
  //
  //  override def parseBytes(bytes: Array[Byte]): Try[AssetBox] = Try {
  //
  //    val params = noncedBoxParseBytes(bytes)
  //
  //    val dataLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
  //    val data: String = new String(bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES))
  //
  //    val assetLen = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES - Ints.BYTES - dataLen,
  //      bytes.length - Ints.BYTES - dataLen))
  //    val asset: String = new String(bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen,
  //      bytes.length - (2 * Ints.BYTES) - dataLen))
  //
  //    val issuer: PublicKey25519Proposition = PublicKey25519Proposition(
  //      bytes.slice(bytes.length - (2 * Ints.BYTES) - dataLen - assetLen - Constants25519.PubKeyLength,
  //        bytes.length - (2 * Ints.BYTES) - dataLen - assetLen)
  //    )
  //
  //    AssetBox(params._1, params._2, params._3, asset, issuer, data)
  //  }
}