package bifrost.modifier.box

import bifrost.modifier.box.proposition.{PublicKey25519Proposition, PublicKey25519PropositionSerializer}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

/**
  * Created by cykoz on 5/15/2017.
  */

 class NoncedBox(override val proposition: PublicKey25519Proposition,
                 override val nonce: Long,
                 override val value: Long) extends Box(proposition, nonce, value) {

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val typeOfBox: String = "NoncedBox"

  lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.toString.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson
}

object NoncedBoxSerializer extends BifrostSerializer[NoncedBox] {

  override def serialize(obj: NoncedBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    w.putULong(obj.value)
  }

  override def parse(r: Reader): NoncedBox = {
    new NoncedBox(
      PublicKey25519PropositionSerializer.parse(r),
      r.getLong(),
      r.getULong()
    )
  }

// TODO: Jing - remove
//
//  def noncedBoxToBytes(obj: NoncedBox, boxType: String): Array[Byte] = {
//    Bytes.concat(
//      Ints.toByteArray(boxType.getBytes.length),
//      boxType.getBytes,
//      obj.proposition.pubKeyBytes,
//      Longs.toByteArray(obj.nonce),
//      Longs.toByteArray(obj.value)
//    )
//  }
//
//  def noncedBoxParseBytes(bytes: Array[Byte]): (PublicKey25519Proposition, Long, Long) = {
//
//    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
//    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))
//
//    require(typeStr.nonEmpty)
//
//    val numReadBytes = Ints.BYTES + typeLen
//
//    val pk = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))
//    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + Constants25519.PubKeyLength,
//                                                numReadBytes + Constants25519.PubKeyLength + Longs.BYTES))
//
//    val curReadBytes = numReadBytes + Constants25519.PubKeyLength + Longs.BYTES
//
//    val value = Longs.fromByteArray(bytes.slice(curReadBytes, curReadBytes + Longs.BYTES))
//    (pk, nonce, value)
//  }
}
