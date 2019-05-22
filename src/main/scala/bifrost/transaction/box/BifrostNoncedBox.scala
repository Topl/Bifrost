package bifrost.transaction.box

import com.google.common.primitives.{Bytes, Ints, Longs}
import io.circe.Json
import io.circe.syntax._
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.crypto.encode.Base58

/**
  * Created by cykoz on 5/15/2017.
  */

abstract class BifrostPublic25519NoncedBox(override val proposition: PublicKey25519Proposition,
                                           override val nonce: Long,
                                           override val value: Long) extends BifrostBox(proposition, nonce, value) {

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val typeOfBox: String = "BifrostPublic25519NoncedBox"

  lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.toString.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson

}

trait NoncedBoxSerializer {

  def noncedBoxToBytes(obj: BifrostPublic25519NoncedBox, boxType: String): Array[Byte] = {
    Bytes.concat(
      Ints.toByteArray(boxType.getBytes.length),
      boxType.getBytes,
      obj.proposition.pubKeyBytes,
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.value)
    )
  }

  def noncedBoxParseBytes(bytes: Array[Byte]): (PublicKey25519Proposition, Long, Long) = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    val numReadBytes = Ints.BYTES + typeLen

    val pk = PublicKey25519Proposition(bytes.slice(numReadBytes, numReadBytes + Constants25519.PubKeyLength))
    val nonce = Longs.fromByteArray(bytes.slice(numReadBytes + Constants25519.PubKeyLength,
                                                numReadBytes + Constants25519.PubKeyLength + Longs.BYTES))

    val curReadBytes = numReadBytes + Constants25519.PubKeyLength + Longs.BYTES

    val value = Longs.fromByteArray(bytes.slice(curReadBytes, curReadBytes + Longs.BYTES))
    (pk, nonce, value)
  }
}
