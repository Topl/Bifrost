package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.ProfileTransaction
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints}
import io.circe.optics.JsonPath.root
import io.circe.parser.parse
import io.circe.{HCursor, Json}
import scorex.crypto.encode.Base58

import scala.util.Try

object ProfileTransactionCompanion extends Serializer[ProfileTransaction] {
  override def toBytes(m: ProfileTransaction): Array[Byte] = {
    val typeBytes = "ProfileTransaction".getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      m.json.toString().getBytes()
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ProfileTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val json: Json = parse(new String(bytesWithoutType)).getOrElse(Json.Null)
    val cursor: HCursor = json.hcursor

    val from = PublicKey25519Proposition(Base58.decode(root.from.string.getOption(json).get).get)
    val fee = root.fee.long.getOption(json).get
    val timestamp = root.timestamp.long.getOption(json).get
    val signature = Signature25519(Base58.decode(root.signature.string.getOption(json).get).get)
    val keyValues: Map[String, String] = cursor.downField("keyValues").as[Map[String, String]].getOrElse(Map())

    ProfileTransaction(from, signature, keyValues, fee, timestamp)
  }
}
